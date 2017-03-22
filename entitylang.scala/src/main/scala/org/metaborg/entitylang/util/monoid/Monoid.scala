package org.metaborg.entitylang.util.monoid

trait Monoid[T] {
  def zero: T
  def append(t1: T, t2: T): T
}

object Monoid{
  implicit def seqMonoid[T] = new SeqMonoid[T]
  implicit def mapMonoid[K,V] = new MapMonoid[K,V]
}
