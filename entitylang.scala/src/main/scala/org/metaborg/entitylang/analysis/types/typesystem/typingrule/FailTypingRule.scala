package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class FailTypingRule[TermType0 <: HasOrigin, TypeType0, T0](val term: HasOrigin, message: String) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0

  override def run(implicit typeSystem: TypeSystemT): TypingResult = typeError(term, message)
}
