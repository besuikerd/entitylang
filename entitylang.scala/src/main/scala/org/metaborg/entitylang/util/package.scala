package org.metaborg.entitylang

import javafx.beans.binding.MapExpression

package object util {
//  @inline implicit def seqValueMapExtensions[K, V](m: Map[K, Seq[V]]): SeqValueMapExtensions[K, V] = new SeqValueMapExtensions(m)
  @inline implicit def seqExtensions[T](s: Seq[T]): SeqExtensions[T] = new SeqExtensions(s)
  @inline implicit def richMap[K,V](map: Map[K, V]): RichMap[K,V] = new RichMap[K, V](map)
  @inline implicit def richString(s: String): RichString = new RichString(s)
  @inline implicit def eitherExtensions[L, R](e: Either[L, R]): EitherExtensions[L, R] = new EitherExtensions(e)
}
