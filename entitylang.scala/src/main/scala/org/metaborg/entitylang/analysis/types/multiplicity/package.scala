package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.math.Bounded

package object multiplicity {
  val zeroToZero = ExactlyZero()
  val oneToOne = ExactlyOne()
  val zeroToOne = ZeroOrOne()
  val oneToMany = OneOrMore()
  val zeroToMany = ZeroOrMore()
}