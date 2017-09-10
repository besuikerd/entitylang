package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

class TermTypingRuleWrapper[TermType <: HasOrigin, TypeType, T](rule: TypingRule[TermType, TypeType, T], val term: HasOrigin) extends TermTypingRule[TermType, TypeType, T]{
  override def run(implicit typeSystem: TypeSystemT): Result = rule.run
}
