package asn1.decoding

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.FlatSpec
import asn1.BitString
import asn1.BitString.Containing
import asn1.BitString.Literal
import asn1.Integer
import General.fromHexString

class DERDecodeBitStringSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("template", "encoding"),
      (BitString(Containing(Integer(1))), "030400020101"),
      (BitString(Literal(Seq(0x7f, 0x77), 0)), "0303007F77"),
      (BitString(Literal(Seq(0x7f, 0x70), 4)), "0303047F70")
      )

  "A encoding of a BIT STRING tag" should "decode to the corresponding template" in {
    forAll(encodingValue) { (template, encoding) =>
       assert(DER.decode(fromHexString(encoding), template) === Success())
    }
  }

  "A 'Containing' template for a BIT STRING" should "not accept non-zero unused bits" in {
    assert(DER.decode(fromHexString("030401020101"), BitString("test", Containing(Integer(1)))) ===
      Failure("Content is not a whole number of bytes: number of unused bits is 1 (expected a BitString 'test' containing a Integer)"))
  }
  
  it should "not consume more than its contentlength" in {
    val content = fromHexString("030400020101FF")  
    val template = BitString(Containing(Integer(1)))

    assert(DER.decode(content, template) === Success())
    assert(content.next === 0xff.toByte)
  }
  
  "A 'Literal' template for a BIT STRING" should "not accept non-matching number of unused bits" in {
      assert(DER.decode(fromHexString("0303047F70"), BitString("test", Literal(Seq(0x7f, 0x70), 3))) ===
        Failure("Expected BitString 'test' with 3 bits unused, but encountered a value of 4 instead"))
  }
  
  it should "not accept non-matching content" in {
      assert(DER.decode(fromHexString("0303047F70"), BitString("test", Literal(Seq(0x7f, 0x71), 4))) ===
        Failure("The BitString 'test' did not have the expected value (got 0x7F70, but expected 0x7F71)"))
  }
  
  it should "not consume more than its contentlength" in {
    val content = fromHexString("0303007F77FF")  
    
    assert(DER.decode(content, BitString(Literal(Seq(0x7f, 0x77), 0))) == Success())
    assert(content.next === 0xff.toByte)    
  }
  
}