package org.metaborg.entitylang.analysis.types.typesystem.typingrule

class FailTypingRule[TermType0, TypeType0, T0 <: TypeType0](val term: TermType0, message: String) extends TermTypingRule[TermType0, TypeType0, T0]{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  override def run(implicit typeSystem: TypeSystemT): TypingResult = typeError(term, message)
}
