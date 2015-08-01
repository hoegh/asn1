package asn1.oid

import asn1.oid.OID._

abstract class OIDs {
  def apply(subIdent: Int) = this.subIdent(subIdent)
  def subIdent(subIdent: Int) = OID(this, subIdent)

  def idents: Seq[Int]
}

case class OID (parent: OIDs, subIdent: Int) extends OIDs {
  require(subIdent >= 0, s"subIdents cannot be negative (was ${subIdent})")
  
  val idents = parent.idents :+ subIdent
}

case class RootArc private (ident: Int) extends OIDs {
  val idents = Seq(ident)
}

object RootArc {
  val ITU_T = RootArc(0)
  val ISO = RootArc(1)
  val JOINT_ISO_ITU_T = RootArc(2)
}

object OID {
  
  val ITU_T = RootArc.ITU_T 
  val ISO = RootArc.ISO 
  val JOINT_ISO_ITU_T = RootArc.JOINT_ISO_ITU_T 
}

object RSA {
  val rsadsi = ISO(2)(840)(113549)
  val pkcs = rsadsi(1)
  val pkcs_1 = pkcs(1)
  val rsaEncryption = pkcs_1(1)
}