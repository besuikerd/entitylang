package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem

import scala.reflect.ClassTag

class EnrichedTermTypingRule[TermType, TypeType, Term <: TermType](val term: Term)(implicit typeSystem: TypeSystem[TermType, TypeType]){
  type Rule[T <: TypeType] = TypingRule.Aux[TermType, TypeType, T]

  def hasType[T <: TypeType](t: T): Rule[T] = new HasTypeTypingRule(term, t)
  def ofType[T <: TypeType: ClassTag](humanReadableName: String): Rule[T] = new OfTypeTypingRule[TermType, TypeType, T](term, Some(humanReadableName))
  def ofType[T <: TypeType: ClassTag]: Rule[T] = new OfTypeTypingRule[TermType, TypeType, T](term, None)
  def infer: Rule[TypeType] = new InferTypingRule(term)
  def fail(message: String): Rule[TypeType] = new FailTypingRule(term, message)
}