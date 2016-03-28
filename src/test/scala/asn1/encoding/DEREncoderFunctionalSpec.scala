package asn1.encoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1._
import asn1.oid.RSA._
import asn1.BitString.Containing

class DEREncoderFunctionalSpec extends FlatSpec {

  "The DER encoder" should "encode a Subject Public Key Info structure" in {    
    val modulus = "00" +
        "B431980AC4BC62C188AADCB0C8BB3335" +            
        "19D50C64B93D41B296FCF331E16636D0" +            
        "8E561244BA75EBE81C9C5B6670335214" +            
        "C9EC4F91517039DE53851716946EEEF4" +            
        "D56FD5CAB3475E1B0C7BC5CC2B6BC190" +            
        "C316310DBF7AC747778FA021C74CD016" +            
        "6500C10FD7B880E3D2756BC1EA9E5C5C" +            
        "EA7DC1A110BCB8E8351C9E27527E418F"

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

    assert(General.toHexString(DER.encode(spki)) ===
      "30819F"+ // subject public key info sequence
      "300D" + //algorithm sequence
      "06092A864886F70D010101"+ //OID rsaEncryption
      "0500"+ // parameters null
      "03818D00"+ // public key info bit string
      "308189"+ // sequence
      "028181"+modulus + //modulus
      "0203010001") //exponent
  }
}