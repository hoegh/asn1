

package asn1

case class Tag(
    val tagClass: Tag.TagClass,
    val tagType: Tag.TagType,
    val tagValue: Int) {
  require(tagValue > 0, s"Tags must have positive tagvalues (was $tagValue)")
  require(tagClass != Tag.UNIVERSAL || tagValue < 31, s"Universal tags cannot have tagvalues greater than 30 (was $tagValue)")
}

object Tag {
  sealed abstract class TagClass(
      val name: String) {
    override def toString = name
  }
  
  case object UNIVERSAL extends TagClass("Universal")
  case object APPLICATION extends TagClass("Application")
  case object CONTEXT_SPECIFIC extends TagClass("Context-specific")
  case object PRIVATE extends TagClass("Private")
  
  sealed abstract class TagType(
      val name: String) {
    override def toString = name
  }
  
  case object PRIMITIVE extends TagType("Primitive")
  case object CONSTRUCTED extends TagType("Constructed")
  
  val Int = Tag(UNIVERSAL, PRIMITIVE, 2)
  val BitString = Tag(UNIVERSAL, PRIMITIVE, 3)
  val OctetString = Tag(UNIVERSAL, PRIMITIVE, 4)
  val Null = Tag(UNIVERSAL, PRIMITIVE, 5) 
  val ObjectIdentifier = Tag(UNIVERSAL, PRIMITIVE, 6)
  val Sequence = Tag(UNIVERSAL, CONSTRUCTED, 16)
  val Set = Tag(UNIVERSAL, CONSTRUCTED, 17)
  val PrintableString = Tag(UNIVERSAL, PRIMITIVE, 19)
  val T61String = Tag(UNIVERSAL, PRIMITIVE, 20)
  val IA5String	= Tag(UNIVERSAL, PRIMITIVE, 22)
  val UTCTime = Tag(UNIVERSAL, PRIMITIVE, 23)

}