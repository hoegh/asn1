package asn1.encoding

import asn1.Tag
import org.scalacheck.Gen
import org.scalacheck.Prop.{ forAll, BooleanOperators, all }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.FlatSpec

class DERTagEncodingSpec extends FlatSpec with GeneratorDrivenPropertyChecks {
  val validTagClasses = Gen.oneOf(Tag.UNIVERSAL, Tag.APPLICATION, Tag.CONTEXT_SPECIFIC, Tag.PRIVATE)
  val validTagTypes = Gen.oneOf(Tag.PRIMITIVE, Tag.CONSTRUCTED)

  def validTags(minTag: Int, maxTag: Int) = for {
    c <- validTagClasses
    t <- validTagTypes
    v <- Gen.choose(minTag, maxTag) suchThat (_ < 31 || c != Tag.UNIVERSAL)
  } yield Tag(c, t, v)

  "A tag" should "start with bit 7-8 encoding class in first byte" in {
    forAll(validTags(1, 255)) { (tag: Tag) =>
      val expectedClassBits = tag.tagClass match {
        case Tag.UNIVERSAL => 0
        case Tag.APPLICATION => 0x40
        case Tag.CONTEXT_SPECIFIC => 0x80
        case Tag.PRIVATE => 0xc0
      }

      val firstByte = DER.encodeTag(tag).next
      val classBits = (firstByte & 0xc0)

      assert(classBits === expectedClassBits, s"firstByte=${firstByte}")
    }
  }

  it should "encode type in bit 6 in first byte" in {
    forAll(validTags(1, 255)) { (tag: Tag) =>
      val expectedTypeBit = tag.tagType match {
        case Tag.PRIMITIVE => 0
        case Tag.CONSTRUCTED => 0x20
      }

      val firstByte = DER.encodeTag(tag).next
      val typeBit = (firstByte & 0x20)

      assert(typeBit === expectedTypeBit, s"firstByte=${firstByte}")
    }
  }

  it should "encode small values in first byte" in {
    forAll(validTags(1, 31)) { (tag: Tag) =>
      val firstByte = DER.encodeTag(tag).next
      val simpleValue = firstByte & 0x1f

      assert(tag.tagValue === simpleValue, s"firstByte=${firstByte}")
    }
  }
  
  it should "encode lesser tag values in two bytes" in {
    forAll(validTags(32, 127)) { (tag: Tag) =>
      val encoded = DER.encodeTag(tag)
      val firstByte = encoded.next
      val simpleValue = firstByte & 0x1f
      
      assert(simpleValue === 0x1f, s"firstByte=${firstByte} must have bit 4-0 all set")
      
      assert(encoded.hasNext, "more than one byte expected")
      val nextByte = encoded.next
      assert(nextByte == tag.tagValue, "nextByte")      
    }
  }
  
  it should "encode greater tag values in multiple bytes" in {
    forAll(validTags(128, 0xffff)) { (tag: Tag) =>
      
      val encoded = DER.encodeTag(tag)
      val firstByte = encoded.next
      val simpleValue = firstByte & 0x1f
      
      assert(simpleValue === 0x1f, s"firstByte=${firstByte} must have bit 4-0 all set")

      val tagEncoding = encoded.toArray
      val expectedEncoding = General.highBitSequenceEncoding(tag.tagValue).toArray 
      assert(tagEncoding === expectedEncoding, s"tag=${tag.tagValue} should be a high bit sequence")
    }
  }
  
}