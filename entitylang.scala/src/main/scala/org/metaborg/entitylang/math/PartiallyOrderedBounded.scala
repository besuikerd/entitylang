package org.metaborg.entitylang.math

trait PartiallyOrderedBounded[T] { this: Bounded[T] =>
  val partialOrdering: PartialOrdering[Bounded[T]]

  def tryCompare(that: Bounded[T]): Option[Int] = partialOrdering.tryCompare(this, that)
  def >(that: Bounded[T]): Boolean = partialOrdering.gt(this, that)
  def >=(that: Bounded[T]): Boolean = partialOrdering.gteq(this, that)
  def <(that: Bounded[T]): Boolean = partialOrdering.lt(this, that)
  def <=(that: Bounded[T]): Boolean = partialOrdering.lteq(this, that)
  def ===(that: Bounded[T]): Boolean = partialOrdering.eq(this, that)
  def !==(that: Bounded[T]): Boolean = partialOrdering.reverse.eq(this, that)
}
