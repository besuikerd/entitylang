package org.metaborg.entitylang.analysis.types.typesystem.typingrule

class FlatMappedTypingRule[TermType0, TypeType0, T <: TypeType0, U <: TypeType0](rule: TypingRule.Aux[TermType0, TypeType0, T], f: T => TypingRule.Aux[TermType0, TypeType0, U]) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = U
  override def run(implicit typeSystem: TypeSystemT): TypingResult = rule.run.right.flatMap(t => f(t).run)
}