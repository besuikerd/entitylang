package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{FailTypingRule, MatchingTypingRule, RichTypingRules, SuccessTypingRule}

package object typesystem {
  type TopLevelTypingRule[TermType0, TypeType0] = PartialFunction[TermType0, TypingRule{type TermType = TermType0; type TypeType = TypeType0}]

  def matching[TermType, TypeType](terms: TermType*)(implicit typeSystem: TypeSystem[TermType, TypeType]): TypingRule.Aux[TermType, TypeType, TypeType] = new MatchingTypingRule[TermType, TypeType](terms:_*)

  def success[TermType, TypeType, T <: TypeType](t: T)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new SuccessTypingRule[TermType, TypeType, T](t)

  def fail[TermType, TypeType, Term <: TermType](term: Term, message: String)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new FailTypingRule[TermType, TypeType](term, message)

  @inline implicit def richTypingRules[TermType, TypeType](term: TermType)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new RichTypingRules(term)
}
