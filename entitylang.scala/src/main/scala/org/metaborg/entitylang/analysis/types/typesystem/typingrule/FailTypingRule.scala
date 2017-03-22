package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class FailTypingRule[TermType0 <: HasOrigin, TypeType0, T0](val term: HasOrigin, message: String) extends TermTypingRule[TermType0, TypeType0, T0]{
  override def run(implicit typeSystem: TypeSystemT): Result = typeError(term, message)
}
