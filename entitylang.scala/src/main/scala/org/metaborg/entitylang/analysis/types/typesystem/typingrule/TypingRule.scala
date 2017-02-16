package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.entitylang.analysis.types.typesystem.error.{GeneralTypeError, MismatchedTypeError, TypeError}

trait TypingRule {
  type TermType
  type TypeType
  type T <: TypeType

  type Rule[U <: TypeType] = TypingRule.Aux[TermType, TypeType, U]

  type TypeSystemT = TypeSystem[TermType, TypeType]
  type TypingResult = Either[TypeError, T]

  def widen[U >: T <: TypeType] = this.asInstanceOf[TypingRule.Aux[TermType, TypeType, U]]

  def typeError(term: TermType, msg: String)(implicit typeSystem: TypeSystemT): TypingResult = Left(GeneralTypeError(typeSystem.getOrigin(term), msg))
  def typeError(term: TermType, expected: TypeType, got: TypeType)(implicit typeSystem: TypeSystemT) = new MismatchedTypeError(typeSystem.getOrigin(term), expected, got)
  def internalError(msg: String) = Left(GeneralTypeError(null, msg))

  def run(implicit typeSystem: TypeSystemT): TypingResult

  def bind(name: String, t: TypeType) = this

  def filter(f: T => Boolean)(implicit typeSystem: TypeSystemT): Rule[T] = new FilteredTypingRule[TermType, TypeType, T](this, f)
  def map[U <: TypeType](f: T => U)(implicit typeSystem: TypeSystemT): Rule[U] = new MappedTypingRule[TermType, TypeType, T, U](this, f)
  def flatMap[U <: TypeType](f: T => Rule[U])(implicit typeSystem: TypeSystemT): Rule[U] = new FlatMappedTypingRule[TermType, TypeType, T, U](this, f)
}

object TypingRule{
  type Aux[TermType0, TypeType0, T0 <: TypeType0] = TypingRule{
    type TermType = TermType0
    type TypeType = TypeType0
    type T = T0
  }
}