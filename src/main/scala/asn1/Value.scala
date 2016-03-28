package asn1

import asn1.oid.OID
import asn1.oid.OID._

import scala.language.implicitConversions

abstract class Value[T]() {}

case class Constant[T](value: T) extends Value[T] {}
case class Placeholder[T]() extends Value[T] {}
