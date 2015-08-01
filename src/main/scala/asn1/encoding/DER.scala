package asn1.encoding

import asn1.Tag
import asn1.Tag.TagClass
import asn1.Tag.TagType
import asn1.Asn1Type
import asn1.Integer
import asn1.Sequence
import asn1.ObjectIdentifier
import asn1.BitString
import asn1.BitString.Literal
import asn1.BitString.Containing
import asn1.Null

object DER {
  def encodeTag(tag: Tag) = {
    def tagClassOffset(tagClass: TagClass) = {
      val classId = tagClass match {
        case Tag.UNIVERSAL => 0
        case Tag.APPLICATION => 1
        case Tag.CONTEXT_SPECIFIC => 2
        case Tag.PRIVATE => 3
      }

      classId << 6 //bit 7+8
    }

    def tagTypeOffset(tagType: TagType) = {
      val tagId = tagType match {
        case Tag.PRIMITIVE => 0
        case Tag.CONSTRUCTED => 1
      }

      tagId << 5 //bit 6
    }

    val offset = tagClassOffset(tag.tagClass) + tagTypeOffset(tag.tagType)
    
    val longFormMarker = 0x1f
    
    if (tag.tagValue < longFormMarker) {
      val value =  offset + tag.tagValue;
      Iterator(value.toByte)
    } else {
      val value = offset + longFormMarker;
      Iterator(value.toByte) ++ General.highBitSequenceEncoding(tag.tagValue)
    }
  }
  
  def encodeLength(length: Int) = {
    require(length >= 0, s"length cannot be negative (was=${length})");
    
    if (length < 128) {
      Iterator(length.toByte) //short form
    } else {
      // long form
      val lenlen = General.octetLength(length)
      Iterator((lenlen+128).toByte) ++ General.octetEncoding(length)
    }    
  }

  def encodeContentWithLength(content: Iterable[Byte]) = {
    val contentAsBytes = content.toArray
    encodeLength(contentAsBytes.length) ++ contentAsBytes
  }

  def encode(element: Asn1Type): Iterator[Byte] = {
    encodeTag(element.tag) ++
      (element match {
        case Integer(value) => encodeContentWithLength(value.toByteArray)

        case Sequence(content) =>
          val encodedContent = (Iterable[Byte]() /: content)(_ ++ encode(_))
          encodeContentWithLength(encodedContent)

        case ObjectIdentifier(oid) =>
          val (root :: (firstChild :: tail)) = oid.idents

          encodeContentWithLength({
              // first two idents (root and first child) are combined before being 7-bit high marked encoded
              val encodedHead = General.highBitSequenceEncoding(root * 40 + firstChild).toIterable
              
              if (tail == Nil)
                encodedHead.toIterable
              else {
                //each ident (lest the first two) is 7-bit high marked encoded before being combined with the rest
                val iter = (encodedHead /: tail)(_ ++ General.highBitSequenceEncoding(_))
                iter.toIterable
              }
          })
          
        case BitString(content) =>
          content match {
            case Literal(bitpattern, unusedbits) => 
              val encoded = Iterator(unusedbits.toByte) ++ bitpattern
              encodeContentWithLength( encoded.toIterable)
            case Containing(element) => 
              val encoded = Iterator(0.toByte) ++ encode(element)
              encodeContentWithLength( encoded.toIterable ) 
          }
          
        case Null() => Iterable(0.toByte)
      })
  }
  
}