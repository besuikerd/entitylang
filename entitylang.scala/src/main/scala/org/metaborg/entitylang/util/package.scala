package org.metaborg.entitylang

package object util {
  @inline implicit def seqValueMapExtensions[K, V](m: Map[K, Seq[V]]): SeqValueMapExtensions[K, V] = new SeqValueMapExtensions(m)
  @inline implicit def seqExtensions[T](s: Seq[T]): SeqExtensions[T] = new SeqExtensions(s)
}
