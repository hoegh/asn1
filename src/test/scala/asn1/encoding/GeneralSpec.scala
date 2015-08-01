package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class GeneralSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      (0, "00"),
      (127, "7F"),
      (128, "8100"),
      (0x3fff, "FF7F"),
      (0x4000, "818000"))

  "A highBitSequence" should "return 7 bit sequence with high bit set for all but last byte" in {
    forAll(encodingValue) { (input, expected) =>
      val encoding = General.highBitSequenceEncoding(input)

      assert(General.toHexString(encoding) === expected)
    }
  }
}