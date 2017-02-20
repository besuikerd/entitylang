package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

class RichTypingRule[TermType <: HasOrigin, TypeType](implicit val typeSystem: TypeSystem[TermType, TypeType]) {

  type Rule[T <: TypeType] = TypingRule.Aux[TermType, TypeType, T]

  def infer[Term <: TermType](term: Term): Rule[TypeType] = new InferTypingRule(term)

  def success[T <: TypeType](t: T): Rule[T] = new SuccessTypingRule[TermType, TypeType, T](t)
  def fail[T <: TypeType](term: HasOrigin, message: String): Rule[T] = new FailTypingRule[TermType, TypeType, T](term, message)
  def mismatchedType[T <: TypeType](term: HasOrigin, expected: T, got: T): Rule[T] = new FailTypingRule[TermType, TypeType, T](term, s"expected type: $expected, got: $got")

  def matching(terms: TermType*): TypingRule.Aux[TermType, TypeType, TypeType] = new MatchingTypingRule[TermType, TypeType](terms:_*)


  def fromTypeEnvironment(term: HasOrigin, name: String)(implicit typeSystem: TypeSystem[TermType, TypeType]): Rule[TypeType] = typeSystem.typeEnvironment.get(name) match {
    case Some(tpe) => success(tpe)
    case None => fail(term, s"Could not find field $name")
  }
}
