package asn1.decoding

import asn1.BitString.Containing
import asn1._
import asn1.oid.RSA._
import org.scalatest.FlatSpec

class DERDecoderFunctionalSpec extends FlatSpec {

  val modulus = "00" +
    "B431980AC4BC62C188AADCB0C8BB3335" +
    "19D50C64B93D41B296FCF331E16636D0" +
    "8E561244BA75EBE81C9C5B6670335214" +
    "C9EC4F91517039DE53851716946EEEF4" +
    "D56FD5CAB3475E1B0C7BC5CC2B6BC190" +
    "C316310DBF7AC747778FA021C74CD016" +
    "6500C10FD7B880E3D2756BC1EA9E5C5C" +
    "EA7DC1A110BCB8E8351C9E27527E418F"

  val spkiData = "30819F"+ // subject public key info sequence
    "300D" + //algorithm sequence
    "06092A864886F70D010101"+ //OID rsaEncryption
    "0500"+ // parameters null
    "03818D00"+ // public key info bit string
    "308189"+ // sequence
    "028181"+modulus + //modulus
    "0203010001" //exponent


  "The DER decoder" should "decode a Subject Public Key Info structure" in {

    val spki =
      Sequence(
        Sequence( "algorithm",
          ObjectIdentifier(rsaEncryption),
          Null()),
        BitString("public key info",
          Containing(
            Sequence(
              Integer("modulus", BigInt(modulus, 16)),
              Integer("exponent", 65537)))))

    assert(DER.decode(General.fromHexString(spkiData), spki) === Success())
  }

  it should "extract data from a Subject Public Key Info structure" in {
    val spki =
      Sequence(
        Sequence( "algorithm",
          ObjectIdentifier("algId"),
          Null()),
        BitString("public key info",
          Containing(
            Sequence(
              Integer("modulus"),
              Integer("exponent")
            )
          )
        )
      )

    val result = DER.decode(General.fromHexString(spkiData), spki)

    assert(result === ResultList(List(
      Result(Some("algId"), rsaEncryption),
      Result(Some("modulus"), BigInt(modulus, 16)),
      Result(Some("exponent"), BigInt(65537))
    )))
  }

  it should "both match and extract from a Subject Public Key Info structure" in {
    val spki =
      Sequence(
        Sequence( "algorithm",
          ObjectIdentifier("algId", rsaEncryption),
          Null()),
        BitString("public key info",
          Containing(
            Sequence(
              Integer("modulus"),
              Integer(BigInt(65537))
            )
          )
        )
      )

    val result = DER.decode(General.fromHexString(spkiData), spki)

    assert(result === Result(Some("modulus"), BigInt(modulus, 16)))
  }
}