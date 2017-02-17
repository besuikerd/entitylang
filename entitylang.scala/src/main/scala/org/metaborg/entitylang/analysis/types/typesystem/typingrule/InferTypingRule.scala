package org.metaborg.entitylang.analysis.types.typesystem.typingrule

class InferTypingRule[TermType0, TypeType0, Term <: TermType0](val term: Term) extends TermTypingRule[TermType0, TypeType0, TypeType0] {
  override def run(implicit typeSystem: TypeSystemT): TypingResult = typeSystem.infer(term)
}
