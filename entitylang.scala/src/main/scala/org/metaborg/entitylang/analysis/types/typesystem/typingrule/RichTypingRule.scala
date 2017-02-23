package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

class RichTypingRule[TermType <: HasOrigin, TypeType](implicit val typeSystem: TypeSystem[TermType, TypeType]) {

  type Rule[T] = TypingRule.Aux[TermType, TypeType, T]
  type Result[T] = Rule[T]#TypingResult

  def infer[Term <: TermType](term: Term): Rule[TypeType] = new InferTypingRule(term)

  def success[T](t: T): Rule[T] = new ResultTypingRule[TermType, TypeType, T](Right(t))
  def success[T](term: HasOrigin, t: T): TermTypingRule[TermType, TypeType, T] = success(t).bindTerm(term)
  def fail[T](term: HasOrigin, message: String): Rule[T] = new FailTypingRule[TermType, TypeType, T](term, message)
  def mismatchedType[T](term: HasOrigin, expected: T, got: T)(implicit pp: T => String): Rule[T] = new FailTypingRule[TermType, TypeType, T](term, s"expected type: ${pp(expected)}, got: ${pp(got)}")

  def matching(terms: TermType*): TypingRule.Aux[TermType, TypeType, TypeType] = new MatchingTypingRule[TermType, TypeType](terms:_*)

  def result[T](res: Result[T]): Rule[T] = new ResultTypingRule[TermType, TypeType, T](res)

  def fromTypeEnvironment(term: HasOrigin, name: String)(implicit typeSystem: TypeSystem[TermType, TypeType]): TermTypingRule[TermType, TypeType, TypeType] = typeSystem.typeEnvironment.get(name) match {
    case Some(tpe) => success(tpe).bindTerm(term)
    case None => fail(term, s"Could not find field $name").bindTerm(term)
  }

  def all = new AllTypingRuleBuilder[TermType, TypeType]
}