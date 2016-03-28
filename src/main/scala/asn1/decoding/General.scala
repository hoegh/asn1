package asn1.decoding

object General {
  def fromHexString(hex: String) = hex.sliding(2, 2).map(Integer.parseInt(_, 16).toByte)

  def toUnsigned(byte: Byte) = 0xff & byte.asInstanceOf[Int]

  def nextHighBitSequence(content: Iterator[Byte]): Iterator[Byte] = {
    val next = content.next()
    if (next < 0)
      Iterator(next) ++ nextHighBitSequence(content)
    else
      Iterator(next)
  }

  def highBitSequenceDecode(bytes: Iterator[Byte]) = (0 /: bytes)((acc, b) => acc * 128 + (b & 0x7f))

  def sequenceOfHighBitSequenceDecode(bytes: Iterator[Byte]): Seq[Int] =
    if (bytes.isEmpty)
      Nil
    else
      Seq(nextHighBitEncodedValue(bytes)) ++ sequenceOfHighBitSequenceDecode(bytes)

  def nextHighBitEncodedValue(content: Iterator[Byte]) = highBitSequenceDecode(nextHighBitSequence(content))
}