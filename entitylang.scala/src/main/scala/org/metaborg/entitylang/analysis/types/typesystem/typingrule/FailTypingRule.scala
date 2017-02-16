package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypingRule

class FailTypingRule[TermType0, TypeType0](term: TermType0, message: String) extends TypingRule{
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = TypeType0

  override def run(implicit typeSystem: TypeSystemT): TypingResult = typeError(term, message)
}
