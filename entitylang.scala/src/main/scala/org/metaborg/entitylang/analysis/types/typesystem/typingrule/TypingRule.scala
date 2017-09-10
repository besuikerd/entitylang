package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, MismatchedTypeError, TypeError}
import org.metaborg.scalaterms.HasOrigin

import scala.reflect.ClassTag

trait TypingRule[TermType <: HasOrigin, TypeType, T] {
  type Rule[U] = TypingRule[TermType, TypeType, U]
  type Result = Either[Seq[TypeError], TypingResult[TermType, TypeType, T]]
  type TypeSystemT = TypeSystem[TermType, TypeType]


  def typeError(term: HasOrigin, msg: String): Result = Left(Seq(GeneralTypeError(term.origin, msg)))
  def typeError(term: HasOrigin, expected: TypeType, got: TypeType): Result = Left(Seq(new MismatchedTypeError(term.origin, expected, got)))
  def internalError(msg: String): Result = Left(Seq(GeneralTypeError(null, msg)))

  def run(implicit typeSystem: TypeSystem[TermType, TypeType]): Result

  def bindTerm(t: HasOrigin): TermTypingRule[TermType, TypeType, T] = new TermTypingRuleWrapper(this, t)

  def filter[U](f: T => Boolean)(implicit typeSystem: TypeSystemT): Rule[T] = new FilteredTypingRule(this, f)
  def map[U](f: T => U)(implicit typeSystem: TypeSystemT): Rule[U] = new MappedTypingRule[TermType, TypeType, T, U](this, f)
  def flatMap[U](f: T => Rule[U])(implicit typeSystem: TypeSystemT): Rule[U] = new FlatMappedTypingRule[TermType, TypeType, T, U](this, f)
}