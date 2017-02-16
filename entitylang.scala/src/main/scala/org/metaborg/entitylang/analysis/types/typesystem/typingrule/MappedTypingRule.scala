package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem

class MappedTypingRule[TermType0, TypeType0, T <: TypeType0, U <: TypeType0](rule: TypingRule.Aux[TermType0, TypeType0, T], f: T => U)(implicit typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule {
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = U
  override def run(implicit typeSystem: TypeSystemT): TypingResult = rule.run.right.map(t => f(t))
}