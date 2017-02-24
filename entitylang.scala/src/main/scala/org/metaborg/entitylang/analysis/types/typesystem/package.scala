package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.analysis.types.typesystem.typingrule._
import org.metaborg.scalaterms.HasOrigin

package object typesystem {
  type TopLevelTypingRule[TermType0, TypeType0] = TypeSystem[TermType0, TypeType0] => PartialFunction[TermType0, TypingRule.Aux[TermType0, TypeType0, TypeType0]] //TypingRule{type TermType = TermType0; type TypeType = TypeType0}]

  def matching[TermType <: HasOrigin, TypeType](terms: TermType*)(implicit typeSystem: TypeSystem[TermType, TypeType]): TypingRule.Aux[TermType, TypeType, TypeType] = new MatchingTypingRule[TermType, TypeType](terms:_*)

  def success[TermType <: HasOrigin, TypeType, T <: TypeType](t: T)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new ResultTypingRule[TermType, TypeType, T](Right(t))
  def fail[TermType <: HasOrigin, TypeType, Term <: TermType](term: HasOrigin, message: String)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new FailTypingRule[TermType, TypeType, TypeType](term, message)

  def typeRule[TermType <: HasOrigin, TypeType](implicit typeSystem: TypeSystem[TermType, TypeType]) = new RichTypingRule[TermType, TypeType]

  @inline implicit def enrichedTermTypingRule[TermType <: HasOrigin, TypeType, Term <: TermType](term: Term)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new EnrichedTermTypingRule[TermType, TypeType, Term](term)
}
