package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.Integer
import asn1.Sequence
import asn1.BitString
import asn1.BitString.Containing
import asn1.BitString.Literal

class BitStringTagSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      (BitString(Containing(Integer(BigInt(1)))), "030400020101"),
      (BitString(Literal(Seq(0x7f, 0x77), 0)), "0303007F77"),
      (BitString(Literal(Seq(0x7f, 0x70), 4)), "0303047F70")
      )

  "A BIT STRING tag" should "encode the content with unused bits value first" in {
    forAll(encodingValue) { (input, expected) =>
      val encoding = DER.encode(input)

      assert(General.toHexString(encoding) === expected)
    }
  }
}