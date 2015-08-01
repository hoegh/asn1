# asn1
Scala based ASN.1 library

This is not a fully functional library; it is for me experimenting with Scala and writing libraries.

ASN.1 does have all the complexities one can wish for (and then some) and thus it will be sufficiently challenging to make this
a realistic learning experience.

The initial commit should just be able to ASN.1 DER encode a Subject Public Key Info structure like this:

    val spki =
      Sequence(
        Sequence( //algorithm
          ObjectIdentifier(rsaEncryption),
          Null()),
        BitString( //public key info
          Containing(
            Sequence(
              Integer(BigInt(modulus, 16)),
              Integer(65537)))))

    DER.encode(spki)

See DEREncoderFunctionalSpec.scala for details.
