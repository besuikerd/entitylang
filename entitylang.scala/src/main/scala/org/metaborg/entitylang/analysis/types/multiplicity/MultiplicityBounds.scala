package org.metaborg.entitylang.analysis.types.multiplicity

import org.metaborg.entitylang.math.{Bounded, PartialOrderingBounded, PartiallyOrderedBounded}
import Multiplicity.{zero, one, many}

sealed trait MultiplicityBounds extends Bounded[Multiplicity] with PartiallyOrderedBounded[Multiplicity]{
  override val partialOrdering: PartialOrdering[Bounded[Multiplicity]] = Multiplicity.partialOrdering

  override def toString: String = s"[${lowerBound.symbol}, ${upperBound.symbol}]"
}

object MultiplicityBounds{

  val zeroToZero = ExactlyZero()
  val oneToOne = ExactlyOne()
  val zeroToOne = ZeroOrOne()
  val oneToMany = OneOrMore()
  val zeroToMany = ZeroOrMore()


  import scalax.collection.GraphPredef._
  import scalax.collection.GraphEdge.DiEdge
  import scalax.collection.Graph

  val graph = Graph[MultiplicityBounds, DiEdge](
    oneToOne ~> zeroToOne,
    oneToOne ~> oneToMany,
    zeroToZero ~> zeroToOne,
    oneToMany ~> zeroToMany,
    zeroToOne ~> zeroToMany
  )

  def lub(m1: MultiplicityBounds, m2: MultiplicityBounds): MultiplicityBounds = {
    val n1 = graph.get(m1)
    val n2 = graph.get(m2)


    if(m1 == m2)
      m1
    else if(n1.hasSuccessor(n2))
      m2
    else if(n2.hasSuccessor(n1))
      m1
    else
      n1.findSuccessor{n3 =>
        n3.hasPredecessor(n2)
      }.map(_.value).get
  }

  /**
    * upgrades the given bounds to have at least a single element as the lower bound. Upper bound remains unmodified,
    * unless upper bound is equal to zero
    */
  def nonZero(m: MultiplicityBounds): MultiplicityBounds = m match {
    case ExactlyZero() => oneToOne
    case ZeroOrOne() => oneToOne
    case ExactlyOne() => oneToOne
    case ZeroOrMore() => oneToMany
    case OneOrMore() => oneToMany
  }

  def merge(m1: MultiplicityBounds, m2: MultiplicityBounds): MultiplicityBounds = {
    val lower = if(m1.lowerBound == zero && m2.lowerBound == zero) zero else one
    val upper = (m1.upperBound, m2.upperBound) match{
      case (One(), One()) => many
      case (One(), Zero()) => one
      case (Zero(), One()) => one
      case (Many(), _) => many
      case (_, Many()) => many
      case (Zero(), Zero()) => zero
    }
    MultiplicityBounds.unapply(lower, upper).get
  }

  def unapply: (Multiplicity, Multiplicity) => Option[MultiplicityBounds] = {
    case (Zero(), Zero()) => Some(zeroToZero)
    case (Zero(), One()) => Some(zeroToOne)
    case (One(), One()) => Some(oneToOne)
    case (One(), Many()) => Some(oneToMany)
    case (Zero(), Many()) => Some(zeroToMany)
    case _ => None
  }
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
  override val lowerBound: Multiplicity = one
  override val upperBound: Multiplicity = many
}