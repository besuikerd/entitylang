package org.metaborg.entitylang.math

trait Bounded[T]{
  val lowerBound: T
  val upperBound: T
}

class PartialOrderingBounded[T](ordering: Ordering[T]) extends PartialOrdering[Bounded[T]]{
  override def tryCompare(x: Bounded[T], y: Bounded[T]): Option[Int] =
    if(ordering.eq(x.lowerBound, y.lowerBound) && ordering.eq(x.upperBound, y.upperBound))
      Some(0)
    else if(ordering.gteq(x.lowerBound, y.lowerBound) && ordering.lteq(x.upperBound, y.upperBound))
      Some(-1)
    else if(ordering.gteq(y.lowerBound, x.lowerBound) && ordering.lteq(y.upperBound, x.upperBound))
      Some(1)
    else
      None

  override def lteq(x: Bounded[T], y: Bounded[T]): Boolean = tryCompare(x, y).exists(_ <= 0)
}