package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem

class RichTypingRule[TermType, TypeType](implicit val typeSystem: TypeSystem[TermType, TypeType]) {

  def success[T <: TypeType](t: T) = new SuccessTypingRule[TermType, TypeType, T](t)
  def fail[T <: TypeType](term: TermType, message: String) = new FailTypingRule[TermType, TypeType](term, message)
}
