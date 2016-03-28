package asn1.decoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.decoding.General.fromHexString

class DERLengthDecodingSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      ("00", 0),
      ("7F", 127),
      ("8180", 128),
      ("81FF", 255),
      ("820100", 256),
      ("82FFFF", 0xffff),
      ("83010000", 0x10000),
      ("83FFFFFF", 0xFFFFFF),
      ("8401000000", 0x1000000))

  "A length" should "decode" in {
    forAll(encodingValue) { (input, expected) =>
      val len = DER.decodeLength(fromHexString(input))

      assert(len === expected)
    }
  }
  
  it should "not consume the following content when decoding" in {
    forAll(encodingValue) { (input, expected ) =>
      val content = fromHexString(input) ++ Iterator(0xff.toByte)
      val len = DER.decodeLength(content)
      
      assert(len === expected, "len")
      assert(content.toArray === Array(0xff.toByte))
    }
  }
}