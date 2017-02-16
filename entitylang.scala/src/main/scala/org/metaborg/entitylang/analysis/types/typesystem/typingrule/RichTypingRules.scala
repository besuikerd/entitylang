package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.{TypeSystem, TypingRule}

import scala.reflect.ClassTag

class RichTypingRules[TermType, TypeType](val term: TermType)(implicit typeSystem: TypeSystem[TermType, TypeType]){
  type Rule[T <: TypeType] = TypingRule.Aux[TermType, TypeType, T]

  def hasType[T <: TypeType](t: T): Rule[T] = new HasTypeTypingRule(term, t)
  def ofType[T <: TypeType: ClassTag]: Rule[T] = new OfTypeTypingRule[TermType, TypeType, T](term)
  def infer: Rule[TypeType] = new InferTypingRule(term)
  def fail(message: String): Rule[TypeType] = new FailTypingRule(term, message)
}