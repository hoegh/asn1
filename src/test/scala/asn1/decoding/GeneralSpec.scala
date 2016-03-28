package asn1.decoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import General._

class GeneralSpec extends FlatSpec with TableDrivenPropertyChecks {
  val encodingValue =
    Table(
      ("input", "expected"),
      ("00", 0),
      ("7F", 127),
      ("8100", 128),
      ("FF7F", 0x3fff),
      ("818000", 0x4000))

  "A highBitSequence" should "decode into a number" in {
    forAll(encodingValue) { (input, expected) =>
      val parsedInput = fromHexString(input)

      assert(highBitSequenceDecode(parsedInput) === expected)
    }
  }

  val consumeValue =
    Table(
      ("input", "expected", "leftover"),
      ("8100", List(0x81, 0), List()),
      ("7F", List(0x7F), List()),
      ("7F00", List(0x7F), List(0)),
      ("81007F00", List(0x81, 0), List(0x7f, 0)))

  it should "only take bytes up to the first value without high bit set" in {
    forAll(consumeValue) { (input, expected, leftover) =>
      val parsedInput = fromHexString(input)

      assert(nextHighBitSequence(parsedInput).toList === expected.map(_.toByte))
      assert(parsedInput.toList === leftover.map(_.toByte))
    }
  }

  val seqOfHighSeqs = Table(
    ("input", "expected"),
    ("7F", Seq(127)),
    ("81007F", Seq(128, 127)),
    ("7FFF7F", Seq(127, 0x3fff)),
    ("01020304", Seq(1, 2, 3, 4)))

  "A sequence of highBitSequences" should "decode" in {
	  forAll(seqOfHighSeqs) { (input, expected) =>
	  	assert(sequenceOfHighBitSequenceDecode(fromHexString(input)) === expected)
	  }
  }
}