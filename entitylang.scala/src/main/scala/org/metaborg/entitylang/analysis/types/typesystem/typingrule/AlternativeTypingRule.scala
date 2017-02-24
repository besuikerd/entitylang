package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class AlternativeTypingRule[TermType0 <: HasOrigin, TypeType0, T0, T1 <: T0, T2 <: T0](r1: TypingRule.Aux[TermType0, TypeType0, T1], r2: TypingRule.Aux[TermType0, TypeType0, T2]) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  override def run(implicit typeSystem: TypeSystemT): TypingResult =
    r1.run.fold(e => r2.run.left.map(_ => e), Right.apply)
}