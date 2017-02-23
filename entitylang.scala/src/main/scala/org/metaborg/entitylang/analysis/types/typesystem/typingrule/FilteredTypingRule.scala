package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class FilteredTypingRule[TermType0 <: HasOrigin, TypeType0, T0](rule: TypingRule.Aux[TermType0, TypeType0, T0], f: T0 => Boolean) extends TypingRule {
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0
  override def run(implicit typeSystem: TypeSystemT): TypingResult =
    rule.run.right.flatMap{ t => if(f(t)) Right(t) else typeError(null, "Match failed on term")}
}