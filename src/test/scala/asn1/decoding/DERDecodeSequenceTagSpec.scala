package asn1.decoding

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import asn1.Integer
import asn1.Sequence
import General.fromHexString

class DERDecodeSequenceTagSpec extends FlatSpec with TableDrivenPropertyChecks {
  val examples =
    Table(
      ("input", "expected"),
      ("3003020101", Sequence(Integer(1))),
      ("3006020101020102", Sequence(Integer(1), Integer(2))))

  "A Sequence" should "decode" in {
    forAll(examples) { (input, expected) =>
      assert(DER.decode(fromHexString(input), expected) === Success())
    }
  }

  val faultyExamples = 
    Table(
      ("input", "expected"),
      ("3006020101020102", Sequence(Integer(2), Integer(2))),
      ("3006020102020101", Sequence(Integer(2), Integer(2)))
      )
  
  it should "report errors" in {
    val rootCause = DER.decode(fromHexString("020101"), Integer(2))
    
    forAll(faultyExamples) { (input, expected) =>
      assert(DER.decode(fromHexString(input), expected) === rootCause)
    }
  }
  
  it should "not consume more than its length allows" in {
    val content = fromHexString("3003020201FF")
    val result = DER.decode(content, Sequence(Integer(1)))
    
    assert(result == Success())
    assert(content.toArray === Array(0xff.toByte))
  }
  
  it should "not succeed if its elements does not consume all of content" in {
    val content = fromHexString("3004020101FF")
    
    val result = DER.decode(content, Sequence(Integer(1)))
    
    assert(result === Failure("Content after last element in Sequence encountered"))
  }

}