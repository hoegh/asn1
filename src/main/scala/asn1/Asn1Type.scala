package asn1

import asn1.oid.OID

abstract class Asn1Type (val tag: Tag, val name: Option[String])

trait Named[T, TT] {
  def apply(name: Option[String], value: T): TT
  def apply(name: String, value: T): TT = apply(Some(name),value)
  def apply(value: T):TT = apply(None, value)
}

trait Valued[T, TT] {
  def apply(name: Option[String], value: Value[T]): TT
  def apply(name: Option[String], value: T): TT = apply(name, Constant(value))
  def apply(name: String, value: T): TT = apply(Some(name), Constant(value))
  def apply(value: T):TT = apply(None, Constant(value))

  def apply(name: String):TT = apply(Some(name), Placeholder[T]())
}

case class Integer(override val name: Option[String], value: Value[BigInt])
  extends Asn1Type(Tag.Int, name)

object Integer extends Named[Value[BigInt], Integer] with Valued[BigInt, Integer]

case class BitString(override val name: Option[String], content: Content)
  extends Asn1Type(Tag.BitString, name)

abstract class Content
object BitString extends Named[Content, BitString] {

  case class Literal(value: Seq[Byte], unusedBits: Int) extends Content {
    require(unusedBits >= 0 && unusedBits <8, s"unusedBits must be between 0 and 7 (was $unusedBits)")
    require(unusedBits == 0 || (value.last & (1 << unusedBits - 1)) == 0, s"when there are unused bits, they must be zero ($unusedBits unused bits, but last byte was 0x${value.last.toHexString})")
  }
  
  case class Containing(element: Asn1Type) extends Content  
}

//case class OctetString(value: Iterable[Byte]) 
//  extends Asn1Type(Tag.OctetString)

case class Null(override val name: Option[String])
  extends Asn1Type(Tag.Null, name)

object Null {
  def apply(): Null = apply(None)
  def apply(name: String): Null = apply(Some(name))
}

case class ObjectIdentifier(override val name: Option[String], oid: Value[OID])
  extends Asn1Type(Tag.ObjectIdentifier, name)

object ObjectIdentifier extends Named[Value[OID], ObjectIdentifier] with Valued[OID, ObjectIdentifier]

case class Sequence(override val name: Option[String], elements: Iterable[Asn1Type])
  extends Asn1Type(Tag.Sequence, name)

object Sequence extends Named[Iterable[Asn1Type], Sequence] {
  def apply(elements: Asn1Type*): Sequence = Sequence(None, elements.toList)
  def apply(name: String, elements: Asn1Type*): Sequence = Sequence(Some(name), elements.toList)
}


//case class Set(elements: Iterable[Asn1Type]) 
//  extends Asn1Type(Tag.Set)

//PrintableString	19	13
//T61String	20	14
//IA5String	22	16
//UTCTime	23	17




