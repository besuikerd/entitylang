package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class AlternativeTypingRule[TermType <: HasOrigin, TypeType, T0, T1 <: T0, T2 <: T0](r1: TypingRule[TermType, TypeType, T1], r2: TypingRule[TermType, TypeType, T2]) extends TypingRule[TermType, TypeType, T0]{
  override def run(implicit typeSystem: TypeSystemT): Result =
    r1.run.left.flatMap(e => r2.run.left.map(_ => e))
}
