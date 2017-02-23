package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class ResultTypingRule[TermType0 <: HasOrigin, TypeType0, T0](result: TypingRule.Aux[TermType0, TypeType0, T0]#TypingResult) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  override def run(implicit typeSystem: TypeSystemT): TypingResult = result
}
