package org.metaborg.entitylang.util

class SeqValueMapExtensions[K,V](val m: Map[K, Seq[V]]) extends AnyVal {
  def addBinding(k: K, v: V): Map[K, Seq[V]] = m.get(k) match{
    case Some(seq) => m.updated(k, v +: seq)
    case None => m + (k -> Seq(v))
  }
}
