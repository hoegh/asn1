package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.Integer
import asn1.Sequence
import asn1.Null

class NullTagSpec extends FlatSpec {

  "A Null tag" should "encode to zero length" in {
      val encoding = DER.encode(Null())

      assert(General.toHexString(encoding) === "0500")
  }
}