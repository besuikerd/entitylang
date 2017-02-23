package org.metaborg.entitylang.analysis.types.typesystem

import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, TypeError}
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{TermTypingRule, TypingRule}
import org.metaborg.scalaterms.{HasOrigin, Origin}

trait TypeSystem[TermType, TypeType]{
  def infer(ast: TermType): TypingRule.Aux[TermType, TypeType, TypeType]#TypingResult
  def typeEnvironment: Map[String, TypeType]

  def withBinding(name: String, t: TypeType): TypeSystem[TermType, TypeType]
  def withBindings(bindings: Map[String, TypeType]): TypeSystem[TermType, TypeType] =
    bindings.foldLeft(this){
      case (typeSystem, (name, t)) => typeSystem.withBinding(name, t)
    }
}


class TypeSystemImpl[TermType <: HasOrigin, TypeType](val rules: Seq[TopLevelTypingRule[TermType, TypeType]], val typeEnvironment: Map[String, TypeType]) extends TypeSystem[TermType, TypeType]{
  override def infer(ast: TermType): TypingRule.Aux[TermType, TypeType, TypeType]#TypingResult =
    rules
      .view
      .flatMap(pf => pf(this).andThen(x => Seq(x.run(this))).applyOrElse(ast, (_: TermType) => Seq.empty))
      .headOption.getOrElse(fail(ast, "Could not find valid typing rule to apply for term " + ast)(this).run(this))

  override def withBinding(name: String, t: TypeType) = new TypeSystemImpl[TermType, TypeType](rules, typeEnvironment + (name -> t))
  override def withBindings(bindings: Map[String, TypeType]): TypeSystem[TermType, TypeType] = new TypeSystemImpl(rules, typeEnvironment ++ bindings)
}

object TypeSystem{
  def apply[TermType <: HasOrigin, TypeType](rules: TopLevelTypingRule[TermType, TypeType]*): TypeSystem[TermType, TypeType] = new TypeSystemImpl(rules, Map.empty)
}