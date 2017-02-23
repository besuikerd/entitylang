package org.metaborg.entitylang.util.monoid

class SeqMonoid[T] extends Monoid[Seq[T]] {
  override def zero = Seq.empty[T]
  override def append(t1: Seq[T], t2: Seq[T]): Seq[T] = t1 ++ t2
}
