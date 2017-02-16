package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem

class SuccessTypingRule[TermType0, TypeType0, T0 <: TypeType0](t1: T0)(implicit  typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  override def run(implicit typeSystem: TypeSystemT): TypingResult = Right(t1)
}
