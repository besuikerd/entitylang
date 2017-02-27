package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.math.Bounded

package object multiplicity {
  val zeroToZero = MultiplicityBounds.zeroToZero
  val oneToOne = MultiplicityBounds.oneToOne
  val zeroToOne = MultiplicityBounds.zeroToOne
  val oneToMany = MultiplicityBounds.oneToMany
  val zeroToMany = MultiplicityBounds.zeroToMany
}