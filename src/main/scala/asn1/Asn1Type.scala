package asn1

import asn1.oid.OID
import asn1.BitString.Content

abstract class Asn1Type (val tag: Tag)

case class Integer(value: BigInt)
  extends Asn1Type(Tag.Int) 

case class BitString(content: Content)
  extends Asn1Type(Tag.BitString) 

object BitString {
  abstract class Content

  case class Literal(value: Seq[Byte], unusedBits: Int) extends Content {
    require(unusedBits >= 0 && unusedBits <8, s"unusedBits must be between 0 and 7 (was ${unusedBits})")
    require(unusedBits == 0 || (value.last & ((1 << unusedBits - 1))) == 0, s"when there are unused bits, they must be zero (${unusedBits} unused bits, but last byte was 0x${value.last.toHexString})")
  }
  
  case class Containing(element: Asn1Type) extends Content  
}

//case class OctetString(value: Iterable[Byte]) 
//  extends Asn1Type(Tag.OctetString)

case class Null() 
  extends Asn1Type(Tag.Null) 

case class ObjectIdentifier(oid: OID) 
  extends Asn1Type(Tag.ObjectIdentifier)

case class Sequence(elements: Iterable[Asn1Type]) 
  extends Asn1Type(Tag.Sequence) 

object Sequence {
  def apply(elements: Asn1Type*): Sequence = Sequence(elements.toList)
}


//case class Set(elements: Iterable[Asn1Type]) 
//  extends Asn1Type(Tag.Set)

//PrintableString	19	13
//T61String	20	14
//IA5String	22	16
//UTCTime	23	17




