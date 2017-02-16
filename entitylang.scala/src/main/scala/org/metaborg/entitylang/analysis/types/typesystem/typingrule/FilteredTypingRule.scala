package org.metaborg.entitylang.analysis.types.typesystem.typingrule

class FilteredTypingRule[TermType0, TypeType0, T0 <: TypeType0](rule: TypingRule.Aux[TermType0, TypeType0, T0], f: T0 => Boolean) extends TypingRule {
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = T0
  override def run(implicit typeSystem: TypeSystemT): TypingResult = rule.run.right.filter(t => f(t)).getOrElse(internalError("Match failed on typing rule"))
}