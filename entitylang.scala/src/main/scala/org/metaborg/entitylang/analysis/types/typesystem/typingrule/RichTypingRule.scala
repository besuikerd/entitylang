package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem

class RichTypingRule[TermType, TypeType](implicit val typeSystem: TypeSystem[TermType, TypeType]) {

  type Rule[T <: TypeType] = TypingRule.Aux[TermType, TypeType, T]

  def infer[Term <: TermType](term: Term): Rule[TypeType] = new InferTypingRule(term)

  def success[T <: TypeType](t: T): Rule[T] = new SuccessTypingRule[TermType, TypeType, T](t)
  def fail[T <: TypeType](term: TermType, message: String): Rule[TypeType] = new FailTypingRule[TermType, TypeType](term, message)
  def mismatchedType(term: TermType, expected: TypeType, got: TypeType): Rule[TypeType] = new FailTypingRule[TermType, TypeType](term, s"expected type: $expected, got: $got")

  def matching(terms: TermType*): TypingRule.Aux[TermType, TypeType, TypeType] = new MatchingTypingRule[TermType, TypeType](terms:_*)


  def fromTypeEnvironment[Term <: TermType](term: Term, name: String): Rule[TypeType] = new FromTypeEnvironmentTypingRule(term, name)
}
