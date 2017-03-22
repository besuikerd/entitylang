package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class FlatMappedTypingRule[TermType <: HasOrigin, TypeType, T, U](rule: TypingRule[TermType, TypeType, T], f: T => TypingRule[TermType, TypeType, U]) extends TypingRule[TermType, TypeType, U]{
  override def run(implicit typeSystem: TypeSystemT): Result = rule.run.right.flatMap{t =>
    f(t.t).run.right.map(t2 => t2.copy(subTypes =  t2.subTypes ++ t.subTypes))
  }
}