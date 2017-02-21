package org.metaborg.entitylang.analysis.types.multiplicity

import org.metaborg.entitylang.math.{Bounded, PartialOrderingBounded, PartiallyOrderedBounded}
import Multiplicity.{zero, one, many}

sealed trait MultiplicityBounds extends Bounded[Multiplicity] with PartiallyOrderedBounded[Multiplicity]{
  override val partialOrdering: PartialOrdering[Bounded[Multiplicity]] = Multiplicity.partialOrdering

  override def toString: String = s"[${lowerBound.symbol}, ${upperBound.symbol}]"
}

case class ExactlyZero() extends MultiplicityBounds{
  override val lowerBound: Multiplicity = zero
  override val upperBound: Multiplicity = zero
}
case class ZeroOrOne() extends MultiplicityBounds{
  override val lowerBound: Multiplicity = zero
  override val upperBound: Multiplicity = one
}
case class ExactlyOne() extends MultiplicityBounds{
  override val lowerBound: Multiplicity = one
  override val upperBound: Multiplicity = one
}
case class ZeroOrMore() extends MultiplicityBounds{
  override val lowerBound: Multiplicity = zero
  override val upperBound: Multiplicity = many
}
case class OneOrMore() extends MultiplicityBounds{
  override val lowerBound: Multiplicity = zero
  override val upperBound: Multiplicity = many
}