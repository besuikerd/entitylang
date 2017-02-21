package org.metaborg.entitylang.analysis.types.typesystem

import org.metaborg.entitylang.analysis.types.typesystem.error.TypeError
import org.metaborg.scalaterms.HasOrigin

class SimpleTypeSystem[TermType <: HasOrigin, TypeType](rule: TopLevelTypingRule[TermType, TypeType], val typeEnvironment: Map[String, TypeType]) extends TypeSystem[TermType, TypeType] {
  def this(rule: TopLevelTypingRule[TermType, TypeType]) = this(rule, Map.empty)

  override def infer(ast: TermType): Either[TypeError, TypeType] = rule(this)(ast).run(this)

  override def withBinding(name: String, t: TypeType): TypeSystem[TermType, TypeType] = new SimpleTypeSystem(rule, typeEnvironment + (name -> t))
}
