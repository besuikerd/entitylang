package org.metaborg.entitylang.analysis.types.multiplicity

import org.metaborg.entitylang.math.{Bounded, PartialOrderingBounded}

sealed abstract class Multiplicity(val symbol: String)
case class Zero() extends Multiplicity("0")
case class One() extends Multiplicity("1")
case class Many() extends Multiplicity("*")


object Multiplicity{
  val ordering: Ordering[Multiplicity] = new MultiplicityOrdering
  val partialOrdering: PartialOrdering[Bounded[Multiplicity]] = new PartialOrderingBounded(ordering)

  val zero = Zero()
  val one = One()
  val many = Many()
}

class MultiplicityOrdering extends Ordering[Multiplicity]{
  override def compare(x: Multiplicity, y: Multiplicity): Int = orderNumber(x) - orderNumber(y)

  private def orderNumber(m: Multiplicity) = m match {
    case Zero() => 0
    case One() => 1
    case Many() => 2
  }
}

