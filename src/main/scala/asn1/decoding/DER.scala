package asn1.decoding

import asn1._
import General.toUnsigned
import asn1.oid.RootArc
import asn1.oid.OID
import asn1.BitString.Containing
import asn1.BitString.Literal
import asn1.encoding.General.toHexString

object DER {
  def decodeTag(content: Iterator[Byte]) = {
    val longFormMarker = 0x1f

    val firstByte = toUnsigned(content.next())

    val tagClass = firstByte >> 6 match {
      case 0 => Tag.UNIVERSAL
      case 1 => Tag.APPLICATION
      case 2 => Tag.CONTEXT_SPECIFIC
      case 3 => Tag.PRIVATE
    }

    val tagType = firstByte >> 5 & 1 match {
      case 0 => Tag.PRIMITIVE
      case 1 => Tag.CONSTRUCTED
    }

    val initialValue = firstByte & longFormMarker
    val tagValue = if (initialValue == longFormMarker) {
      General.nextHighBitEncodedValue(content)
    } else {
      initialValue
    }

    Tag(tagClass, tagType, tagValue)
  }

  def decodeLength(content: Iterator[Byte]) = {
    val firstByte = toUnsigned(content.next())

    if (firstByte < 0x80) {
      firstByte.toLong
    } else {
      val lenlen = firstByte & 0x7f
      val lenbytes = content.take(lenlen).map(toUnsigned)
      (0L /: lenbytes)(_ * 256 + _)
    }
  }

  def decode(content: Iterator[Byte], templates: Iterator[Asn1Type]): DecodingResult = {
    if (!templates.hasNext)
      Success()
    else {
      val result = decode(content, templates.next())
      result match {
        case Failure(_) => result
        case _ => result.combineWith(decode(content, templates))
      }
    }
  }

  def decode(content: Iterator[Byte], template: Asn1Type): DecodingResult = {
    def formatTag(tag: Tag) = s"$tag=0x${asn1.encoding.General.toHexString(asn1.encoding.DER.encodeTag(tag))}"
    def formatName(element: Asn1Type) = element.name match {
      case None => ""
      case Some(value) => s"'$value' "
    }

    val tag = decodeTag(content)
    if (tag != template.tag)
      Failure(s"Expected ${template.getClass.getSimpleName} tag ${formatName(template)}with value ${formatTag(template.tag)} but encountered ${formatTag(tag)} instead")
    else {
      val len = decodeLength(content)
      val tagContent = content.take(len.toInt)

      val result = template match {
        case Integer(name, value) =>
          val actualValue = BigInt(tagContent.toArray)

          value match {
            case Placeholder() => Result(template.name, actualValue)
            case Constant(templateValue) =>
              if (actualValue == templateValue)
                Success()
              else
                Failure(s"Expected Integer ${formatName(template)}with value $templateValue, but encountered a value of $actualValue instead")
          }


        case Sequence(name, templates) =>
          decode(tagContent, templates.toIterator)

        case ObjectIdentifier(name, value) =>
          def decodeOidStart(value: Int) = {
            // OIDs root and first ident are encode as a single value=root*40+ident (with the caveat that root cannot be greater than 2)
            val rootIdent = value / 40 match {
              case 0 => OID.ITU_T
              case 1 => OID.ISO
              case _ => OID.JOINT_ISO_ITU_T
            }
            val nextIdent = value - 40 * rootIdent.ident

            rootIdent(nextIdent)
          }

          val start :: idents = General.sequenceOfHighBitSequenceDecode(tagContent)
          val oidValue = (decodeOidStart(start) /: idents)(_(_))

          value match {
            case Placeholder() => Result(template.name, oidValue)
            case Constant(oid) =>
              if (oid == oidValue)
                Success()
              else
                Failure(s"Expected ObjectIdentifier ${formatName(template)}with value $oid, but encountered a value of $oidValue instead")
          }

        case BitString(name, bitstringContent) =>
          val unusedBitsValue = tagContent.next()

          bitstringContent match {
            case Literal(value, unusedBits) =>
              if (unusedBits == unusedBitsValue) {
                val contentValue = tagContent.toList
            	if (value == contentValue) Success() else Failure(s"The BitString ${formatName(template)}did not have the expected value (got 0x${toHexString(contentValue.toIterator)}, but expected 0x${toHexString(value.toIterator)})")
              } else
                Failure(s"Expected BitString ${formatName(template)}with $unusedBits bits unused, but encountered a value of $unusedBitsValue instead")
                
            case Containing(element) =>
              if (unusedBitsValue == 0)
                decode(tagContent, element)
              else
                Failure(s"Content is not a whole number of bytes: number of unused bits is $unusedBitsValue (expected a BitString ${formatName(template)}containing a ${element.getClass.getSimpleName})")
          }

        case Null(name) =>
          Success()

        case _ =>
          Failure(s"Tag ${formatName(template)}of type ${template.getClass.getSimpleName} not supported")
      }

      if (!result.isInstanceOf[Failure] && tagContent.hasNext)
        Failure(s"Content after last element in ${template.getClass.getSimpleName} ${formatName(template)}encountered")
      else
        result

    }
  }

}