package asn1.encoding

/**
 * General functions
 */
object General {
  def toHexString(it: Iterator[Byte]) = it.map("%02X".format(_)).mkString

  // returns result little endian
  def baseN(base: Int)(value: Long): List[Byte] = {
    require(base>0, s"base should be positive (was=${base})")
    require(base<=8, s"base cannot be greater then 8 (was=${base})")
    
    val maxBase = 1 << base;

    def helper(value: Long): List[Byte] = {
      if (value < maxBase)
        List(value.toByte)
      else
        (value & (maxBase - 1)).toByte :: helper(value >> base)
    }
    
    helper(value)
  }
  
  val base7 = baseN(7)_
  val base8 = baseN(8)_

  
  /**
   * Returns an iterator of the input broken into 7-bit values with bit8 set for all but the last one. 
   */
  def highBitSequenceEncoding(value: Long) = {
    val digits = if (value < 0x80)
      List(value.toByte)
    else
      (base7(value) match { 
        case (head :: tail) => head :: tail.map(_ | 0x80.toByte).map(_.toByte) }
      ).reverse //convert to bigendian

    digits.toIterator
  }

  def octetLength(input: Long) = {
    def helper(input: Long, result: Int): Int = {
      if (input < 256) {
        result
      } else {
        helper(input / 256, result + 1)
      }
    }

    helper(input, 1)
  }

  def octetEncoding(value: Long) = {
    base8(value).reverse
  }
  
}