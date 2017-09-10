package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class ResultTypingRule[TermType <: HasOrigin, TypeType, T](result: TypingRule[TermType, TypeType, T]#Result) extends TypingRule[TermType, TypeType, T]{
  override def run(implicit typeSystem: TypeSystemT): Result = result
}
