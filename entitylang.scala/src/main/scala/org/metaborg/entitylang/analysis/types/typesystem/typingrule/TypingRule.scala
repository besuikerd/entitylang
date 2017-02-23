package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, MismatchedTypeError, TypeError}
import org.metaborg.scalaterms.HasOrigin

import scala.reflect.ClassTag

trait TypingRule {
  type TermType <: HasOrigin
  type TypeType
  type T

  type Rule[U] = TypingRule.Aux[TermType, TypeType, U]

  type TypeSystemT = TypeSystem[TermType, TypeType]
  type TypingResult = Either[Seq[TypeError], T]

  def typeError(term: HasOrigin, msg: String)(implicit typeSystem: TypeSystemT): TypingResult = Left(Seq(GeneralTypeError(term.origin, msg)))
  def typeError(term: HasOrigin, expected: TypeType, got: TypeType)(implicit typeSystem: TypeSystemT): TypingResult = Left(Seq(new MismatchedTypeError(term.origin, expected, got)))
  def internalError(msg: String): TypingResult = Left(Seq(GeneralTypeError(null, msg)))

  def run(implicit typeSystem: TypeSystemT): TypingResult

  def bindTerm(t: HasOrigin): TermTypingRule[TermType, TypeType, T] = new TermTypingRuleWrapper(this, t)

  def filter(f: T => Boolean)(implicit typeSystem: TypeSystemT): Rule[T] = new FilteredTypingRule[TermType, TypeType, T](this, f)
  def map[U](f: T => U)(implicit typeSystem: TypeSystemT): Rule[U] = new MappedTypingRule[TermType, TypeType, T, U](this, f)
  def flatMap[U](f: T => Rule[U])(implicit typeSystem: TypeSystemT): Rule[U] = new FlatMappedTypingRule[TermType, TypeType, T, U](this, f)
}

object TypingRule{
  type Aux[TermType0, TypeType0, T0] = TypingRule{
    type TermType = TermType0
    type TypeType = TypeType0
    type T = T0
  }
}