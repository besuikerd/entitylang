package org.metaborg.entitylang.analysis.types.typesystem

import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, TypeError}
import org.metaborg.scalaterms.{HasOrigin, Origin}

trait TypeSystem[TermType, TypeType]{
  def infer(ast: TermType): Either[TypeError, TypeType]
  def typeEnvironment: Map[String, TypeType]

  def withBinding(name: String, t: TypeType): TypeSystem[TermType, TypeType]
  def withBindings(bindings: Map[String, TypeType]): TypeSystem[TermType, TypeType] =
    bindings.foldLeft(this){
      case (typeSystem, (name, t)) => typeSystem.withBinding(name, t)
    }
}


class TypeSystemImpl[TermType <: HasOrigin, TypeType](val rules: Seq[TopLevelTypingRule[TermType, TypeType]], val typeEnvironment: Map[String, TypeType]) extends TypeSystem[TermType, TypeType]{
  override def infer(ast: TermType): Either[TypeError, TypeType] =
    rules
      .view
      .flatMap(pf => pf(this).andThen(x => Seq(x.run(this))).applyOrElse(ast, (_: TermType) => Seq.empty))
      .headOption.getOrElse(Left(GeneralTypeError(ast.origin, "Could not find valid typing rule to apply for term " + ast)))

  override def withBinding(name: String, t: TypeType) = new TypeSystemImpl[TermType, TypeType](rules, typeEnvironment + (name -> t))
  override def withBindings(bindings: Map[String, TypeType]): TypeSystem[TermType, TypeType] = new TypeSystemImpl(rules, typeEnvironment ++ bindings)
}

object TypeSystem{
  def apply[TermType <: HasOrigin, TypeType](rules: TopLevelTypingRule[TermType, TypeType]*): TypeSystem[TermType, TypeType] = new TypeSystemImpl(rules, Map.empty)
}