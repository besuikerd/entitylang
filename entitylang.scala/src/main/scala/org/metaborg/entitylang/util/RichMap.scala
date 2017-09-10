package org.metaborg.entitylang.util

import org.metaborg.entitylang.util.monoid.Monoid

class RichMap[K,V](val map: Map[K,V]) extends AnyVal {
  def addBinding(k: K, v: V)(implicit monoid: Monoid[V]) = map.get(k) match{
    case Some(v2) => map.updated(k, monoid.append(v2, v))
    case None => map + (k -> monoid.append(monoid.zero, v))
  }
}
