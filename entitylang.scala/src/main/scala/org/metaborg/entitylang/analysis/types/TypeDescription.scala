package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.analysis.BaseType

class TypeDescription(val t: BaseType) extends AnyVal{
  def hasType(t2: BaseType): Description = if(t == t2) TruthyDescription(t) else FalseDescription(t, t2)
}

object TypeDescription{
  @inline implicit def typeDescription(t: BaseType) = new TypeDescription(t)

}

sealed trait Description {
  def &&(d2: Description): Description = CompositeDescription(this, d2)
  def ==>(t: BaseType): Description = TruthyDescription(t)
}
case class TruthyDescription(t: BaseType) extends Description
case class FalseDescription(expected: BaseType, got: BaseType) extends Description
case class CompositeDescription(d1: Description, d2: Description) extends Description