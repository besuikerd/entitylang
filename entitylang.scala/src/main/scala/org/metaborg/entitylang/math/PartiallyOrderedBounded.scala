package org.metaborg.entitylang.math

trait PartiallyOrderedBounded[T] { this: Bounded[T] =>
  val partialOrdering: PartialOrdering[Bounded[T]]

  def tryCompare(that: Bounded[T]): Option[Int] = partialOrdering.tryCompare(this, that)
  def >(that: Bounded[T]) = partialOrdering.gt(this, that)
  def >=(that: Bounded[T]) = partialOrdering.gteq(this, that)
  def <(that: Bounded[T]) = partialOrdering.lt(this, that)
  def <=(that: Bounded[T]) = partialOrdering.lteq(this, that)
  def ===(that: Bounded[T]) = partialOrdering.eq(this, that)
  def !==(that: Bounded[T]) = partialOrdering.reverse.eq(this, that)
}
