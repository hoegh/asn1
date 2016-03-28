package asn1.decoding

import asn1.{Constant, ObjectIdentifier}
import asn1.oid.OID._
import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.decoding.General.fromHexString

class DERDecodeObjectIdentifierTagSpec extends FlatSpec with TableDrivenPropertyChecks {

  val encodingValue =
    Table(
      ("input", "expected"),
      ("0603813403", ObjectIdentifier(JOINT_ISO_ITU_T(100)(3))), //X.690 8.19.5 example oid={2 100 3}
      ("060128", ObjectIdentifier(ISO(0))), // ISO/Standard
      ("06032B0601", ObjectIdentifier(ISO(3)(6)(1))), //internet aka. 1.3.6.1
      ("06052A81508130", ObjectIdentifier(ISO(2)(208)(176))) //dk.nsi 1.2.208.176
      )

  "An Object Identifier" should "decode" in {
    forAll(encodingValue) { (input, expected) =>
    	assert(DER.decode(fromHexString(input), expected) === Success())
    }
  }
  
  it should "report mismatch" in {
    assert(DER.decode(fromHexString("060128"), ObjectIdentifier(ISO(3)(6)(1))) === Failure(s"Expected ObjectIdentifier with value ${ISO(3)(6)(1)}, but encountered a value of ${ISO(0)} instead"))
  }
}