package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.analysis.types.typesystem.typingrule._
import org.metaborg.scalaterms.HasOrigin

package object typesystem {
  type TopLevelTypingRule[TermType <: HasOrigin, TypeType] = TypeSystem[TermType, TypeType] => PartialFunction[TermType, TypingRule[TermType, TypeType, TypeType]] //TypingRule{type TermType = TermType0; type TypeType = TypeType0}]

  def matching[TermType <: HasOrigin, TypeType](terms: TermType*)(implicit typeSystem: TypeSystem[TermType, TypeType]): TypingRule[TermType, TypeType, TypeType] = new MatchingTypingRule[TermType, TypeType](terms:_*)

  def success[TermType <: HasOrigin, TypeType, T <: TypeType](t: T)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new ResultTypingRule[TermType, TypeType, T](Right(TypingResult(t, Map.empty)))
  def fail[TermType <: HasOrigin, TypeType, Term <: TermType](term: HasOrigin, message: String)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new FailTypingRule[TermType, TypeType, TypeType](term, message)

  def typeRule[TermType <: HasOrigin, TypeType](implicit typeSystem: TypeSystem[TermType, TypeType]) = new RichTypingRule[TermType, TypeType]

  @inline implicit def enrichedTermTypingRule[TermType <: HasOrigin, TypeType, Term <: TermType](term: Term)(implicit typeSystem: TypeSystem[TermType, TypeType]) = new EnrichedTermTypingRule[TermType, TypeType, Term](term)
}
