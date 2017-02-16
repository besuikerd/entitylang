package org.metaborg.entitylang.analysis.types.typesystem.typingrule

class InferTypingRule[TermType0, TypeType0](term: TermType0) extends TypingRule {
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = TypeType0

  override def run(implicit typeSystem: TypeSystemT): TypingResult = typeSystem.infer(term)
}
