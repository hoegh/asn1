package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.Integer
import asn1.Sequence
import asn1.oid.OID._
import asn1.ObjectIdentifier

class ObjectIdentifierTagSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      (ObjectIdentifier(JOINT_ISO_ITU_T(100)(3)), "0603813403"), //X.690 8.19.5 example oid={2 100 3}
      (ObjectIdentifier(ISO(0)), "060128"), // ISO/Standard
      (ObjectIdentifier(ISO(3)(6)(1)), "06032B0601"), //internet aka. 1.3.6.1
      (ObjectIdentifier(ISO(2)(208)(176)), "06052A81508130") //dk.nsi 1.2.208.176
      )

  "An OBJECT IDENTIFIER tag" should "encode the idents as 7-bit high marked" in {
    forAll(encodingValue) { (input, expected) =>
      val encoding = DER.encode(input)

      assert(General.toHexString(encoding) === expected)
    }
  }
}