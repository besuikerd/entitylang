package org.metaborg.entitylang.analysis.types.typesystem.error

import org.metaborg.scalaterms.Origin

case class MismatchedTypeError[T](origin: Origin, expected: T, got: T) extends TypeError{
  override val message: String = s"Expected type: $expected, got: $got"
}