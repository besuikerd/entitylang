package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.{TypeSystem, TypingRule}

class HasTypeTypingRule[TermType0, TypeType0, T0 <: TypeType0](e: TermType0, t1: T0)(implicit typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0
  override def run(implicit typeSystem: TypeSystemT): TypingResult = {
    typeSystem.infer(e).right.flatMap(t2 => if(t1 == t2) Right(t1) else typeError(e, s"Expected type: $t1, got: $t2"))
  }
}
