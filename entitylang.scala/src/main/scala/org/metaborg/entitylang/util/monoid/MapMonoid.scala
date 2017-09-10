package org.metaborg.entitylang.util.monoid

class MapMonoid[K,V] extends Monoid[Map[K, V]]{
  override def zero: Map[K, V] = Map.empty[K,V]
  override def append(t1: Map[K, V], t2: Map[K, V]): Map[K, V] = t1 ++ t2
}
