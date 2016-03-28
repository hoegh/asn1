package asn1.decoding

import asn1.Tag
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, BooleanOperators, all }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.FlatSpec

class DERTagDecodingSpec extends FlatSpec with GeneratorDrivenPropertyChecks {
  val validTagClasses = Gen.oneOf(Tag.UNIVERSAL, Tag.APPLICATION, Tag.CONTEXT_SPECIFIC, Tag.PRIVATE)
  val validTagTypes = Gen.oneOf(Tag.PRIMITIVE, Tag.CONSTRUCTED)

  def validTags(minTag: Int, maxTag: Int) = for {
    c <- validTagClasses
    t <- validTagTypes
    v <- Gen.choose(minTag, maxTag) suchThat (_ < 31 || c != Tag.UNIVERSAL)
  } yield Tag(c, t, v)

  "Small tag" should "decode" in {
    forAll(validTags(1, 255)) { (tag: Tag) =>
      val encodedTag = asn1.encoding.DER.encodeTag(tag)
      val decodedTag = DER.decodeTag(encodedTag)
      
      assert(tag === decodedTag, s"tag=$tag, decoded tag=$decodedTag")
    }
  }

  it should "not consume content while decodig" in {
    forAll(validTags(1, 255)) { (tag: Tag) =>
      val encoded = asn1.encoding.DER.encodeTag(tag) ++ Iterator(0xff.toByte)
      val decodedTag = DER.decodeTag(encoded)
      
      assert(tag === decodedTag, s"tag=$tag, decoded tag=$decodedTag")
      assert(encoded.toArray === Array[Byte](0xff.toByte))
    }
  }

  
  "Larger tags" should "decode as well" in {
    forAll(validTags(128, 0xffff)) { (tag: Tag) =>
      val encodedTag = asn1.encoding.DER.encodeTag(tag)
      val decodedTag = DER.decodeTag(encodedTag)
      
      assert(tag === decodedTag, s"tag=$tag, decoded tag=$decodedTag")
   }
  }
    
  it should "not consume any content when decoding" in {
    forAll(validTags(128, 0xffff)) { (tag: Tag) =>
      val encoded = asn1.encoding.DER.encodeTag(tag) ++ Iterator(0xff.toByte)
      val decodedTag = DER.decodeTag(encoded)
      
      assert(tag === decodedTag, s"tag=$tag, decoded tag=$decodedTag")
      assert(encoded.toArray === Array(0xff.toByte))
   }
  }
  

}