package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.Integer

class IntegerTagSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      (BigInt(0), "020100"),
      (BigInt(127), "02017F"),
      (BigInt(128), "02020080"),
      (BigInt(255), "020200FF"),
      (BigInt(256), "02020100"),
      (BigInt(32767), "02027FFF"),
      (BigInt(32768), "0203008000"),
      (BigInt(-1), "0201FF"),
      (BigInt(-128), "020180"),
      (BigInt(-129), "0202FF7F"),
      (BigInt(-32768), "02028000"),
      (BigInt(-32769), "0203FF7FFF"),
      (BigInt(Long.MaxValue)+1, "0209008000000000000000")
      )

  "An INTEGER tag" should "encode to the twos-complement shortest form with a sign bit" in {
    forAll(encodingValue) { (input, expected) =>
      val encoding = DER.encode(Integer(input))

      assert(General.toHexString(encoding) === expected)
    }
  }
}