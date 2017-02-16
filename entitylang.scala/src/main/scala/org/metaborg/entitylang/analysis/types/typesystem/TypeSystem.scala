package org.metaborg.entitylang.analysis.types.typesystem

import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, TypeError}
import org.metaborg.scalaterms.{HasOrigin, Origin}

trait TypeSystem[TermType, TypeType]{
  def infer(ast: TermType): Either[TypeError, TypeType]
  def getOrigin(t: TermType): Origin
}


class TypeSystemImpl[TermType <: HasOrigin, TypeType](val rules: Seq[TopLevelTypingRule[TermType, TypeType]]) extends TypeSystem[TermType, TypeType]{
  override def infer(ast: TermType): Either[TypeError, TypeType] =
    rules
      .view
      .flatMap(pf => pf.andThen(x => Seq(x.run(this))).applyOrElse(ast, (_: TermType) => Seq.empty))
      .headOption.getOrElse(Left(GeneralTypeError(null, "Could not find valid rule to apply")))

  override def getOrigin(t: TermType): Origin = t.origin
}

object TypeSystem{
  def apply[TermType <: HasOrigin, TypeType](rules: TopLevelTypingRule[TermType, TypeType]*): TypeSystem[TermType, TypeType] = new TypeSystemImpl(rules)
}