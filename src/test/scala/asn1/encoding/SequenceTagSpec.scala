package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.Integer
import asn1.Sequence

class SequenceTagSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      (Sequence(Integer(1)), "3003020101"),
      (Sequence(Integer(1), Integer(2)), "3006020101020102")
      )

  "A SEQUENCE tag" should "encode the content in order" in {
    forAll(encodingValue) { (input, expected) =>
      val encoding = DER.encode(input)

      assert(General.toHexString(encoding) === expected)
    }
  }
}