# asn1
Scala based ASN.1 library.

This library aims at usecases where the ASN.1 structure is know, and aids in encoding and decoding.

It is not well suited for usecases where the structure is unknown.

## Motivation

This is not a fully functional library; it is for me experimenting with Scala and writing libraries.

ASN.1 does have all the complexities one can wish for (and then some) and thus it will be sufficiently challenging to make this
a realistic learning experience.

## Functionality

The library should just be able to ASN.1 DER encode and decode a Subject Public Key Info structure.

### SPKI encode example
(See DEREncoderFunctionalSpec.scala for details)

Names are optional. Values are actually instances of the Constant class, as the example takes advantage of overloaded constructors.

    val spki =
      Sequence(
        Sequence("algorithm",
          ObjectIdentifier(rsaEncryption),
          Null()),
        BitString("public key info",
          Containing(
            Sequence(
              Integer("modulus", BigInt(modulus, 16)),
              Integer("exponent", 65537)))))

    val spkiData = DER.encode(spki)


Note: Placeholders (see below) are meant for decoding and behaviour while encoding will at best be undefined.

### SPKI decode example
(See DERDecoderFunctionalSpec.scala for details)

#### Decoding, all placeholders

Here algorithm, modulus and exponent are instances for the Placeholder class; the example uses the overloaded constructors.

    val spki =
      Sequence(
        Sequence( "algorithm",
          ObjectIdentifier("algId"),
          Null()),
        BitString("public key info",
          Containing(
            Sequence(
              Integer("modulus"),
              Integer("exponent")))))

    val result = DER.decode(spkiData, spki)

    assert(result === ResultList(List(
      Result(Some("algId"), rsaEncryption),
      Result(Some("modulus"), BigInt(modulus, 16)),
      Result(Some("exponent"), BigInt(65537))
    )))

#### Decoding, all constants

Alternatively the decoder can be used to check if a definition matches some data.

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

    assert(DER.decode(spkiData, spki) === Success())

Note: the spki definition here is exactly the same as in the encoder example and hence the following holds:

    assert(DER.decode(DER.encode(spki), spki) === Success())

#### Decoding, mixed placeholders and contants

When decoding, Constants and Placeholders can be mixed as well, making assertion with constants and extracting values with placeholders. 

This definition will assert an RSA public key with a popular exponentvalue and extract the modulus if the assertion holds.

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

    val result = DER.decode(spkiData, spki)

    assert(result === Result(Some("modulus"), BigInt(modulus, 16)))

