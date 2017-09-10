package org.metaborg.entitylang.util.monoid

import org.metaborg.entitylang.util.tuple.FlattenTupleBuilder

object TupleExtensions {
  def flatten = new FlattenTupleBuilder
}
