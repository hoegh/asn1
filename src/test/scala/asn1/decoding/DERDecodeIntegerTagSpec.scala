package asn1.decoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.Integer
import asn1.decoding.General.fromHexString
import asn1.Tag
import asn1.Tag.UNIVERSAL
import asn1.Tag.PRIMITIVE

class DERDecodeIntegerTagSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      ("020100", Integer(0)),
      ("02017F", Integer(127)),
      ("02020080", Integer(128)),
      ("020200FF", Integer(255)),
      ("02020100", Integer(256)),
      ("02027FFF", Integer(32767)),
      ("0203008000", Integer(32768)),
      ("0201FF", Integer(-1)),
      ("020180", Integer(-128)),
      ("0202FF7F", Integer(-129)),
      ("02028000", Integer(-32768)),
      ("0203FF7FFF", Integer(-32769)),
      ("0209008000000000000000", Integer(BigInt(Long.MaxValue)+1))

    )
      
  "An Integer tag" should "decode" in {
    forAll(encodingValue) { (input, expected) =>
      val result = DER.decode(fromHexString(input), expected)

      assert(result === Success())
    }
  }
  
  it should "report wrong tag values" in {
    val result = DER.decode(fromHexString("03"), Integer(0))
    
    assert(result === Failure("Expected Integer tag with value "+Tag(UNIVERSAL, PRIMITIVE, 2)+"=0x02 but encountered "+Tag(UNIVERSAL, PRIMITIVE, 3)+"=0x03 instead"))
  }

  it should "report wrong values" in {
    val result = DER.decode(fromHexString("020101"), Integer(0))
    
    assert(result === Failure("Expected Integer with value 0, but encountered a value of 1 instead"))
  }

  it should "report name in tag error message" in {
    val result = DER.decode(fromHexString("03"), Integer("exponent", 0))
    
    assert(result === Failure("Expected Integer tag 'exponent' with value "+Tag(UNIVERSAL, PRIMITIVE, 2)+"=0x02 but encountered "+Tag(UNIVERSAL, PRIMITIVE, 3)+"=0x03 instead"))
  }
  
  it should "report name in value error message" in {
    val result = DER.decode(fromHexString("020101"), Integer("exponent", 0))
    
    assert(result === Failure("Expected Integer 'exponent' with value 0, but encountered a value of 1 instead"))
  }

  it should "not consume more of the input than its length allows" in {
    val content = fromHexString("020100FF")
    val result = DER.decode(content, Integer(0))
    
    assert(result === Success())
    assert(content.toArray === Array[Byte](0xff.toByte))
    
  }

}