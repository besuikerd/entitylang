package org.metaborg.entitylang.analysis.types.typesystem.typingrule

class TermTypingRuleWrapper[TermType, TypeType, T <: TypeType](rule: TypingRule.Aux[TermType, TypeType, T], val term: TermType) extends TermTypingRule[TermType, TypeType, T]{
  override def run(implicit typeSystem: TypeSystemT): TypingResult = rule.run
}
