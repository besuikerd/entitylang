package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class InferTypingRule[TermType0 <: HasOrigin, TypeType0, Term <: TermType0](val term: Term) extends TermTypingRule[TermType0, TypeType0, TypeType0] {
  override def run(implicit typeSystem: TypeSystemT): Result = typeSystem.infer(term)
}