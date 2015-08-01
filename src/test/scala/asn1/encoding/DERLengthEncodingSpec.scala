package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class DERLengthEncodingSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      (0, "00"),
      (127, "7F"),
      (128, "8180"),
      (255, "81FF"),
      (256, "820100"),
      (0xffff, "82FFFF"),
      (0x10000, "83010000"),
      (0xFFFFFF, "83FFFFFF"),
      (0x1000000, "8401000000"))

  "An encoded length" should "return return result in 8 bit form (with octetlength prepended in long form)" in {
    forAll(encodingValue) { (input, expected) =>
      val encoding = DER.encodeLength(input)

      assert(General.toHexString(encoding) === expected)
    }
  }
}