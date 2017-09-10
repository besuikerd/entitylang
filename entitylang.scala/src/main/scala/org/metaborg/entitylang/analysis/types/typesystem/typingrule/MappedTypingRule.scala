package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

class MappedTypingRule[TermType0 <: HasOrigin, TypeType0, T, U](rule: TypingRule[TermType0, TypeType0, T], f: T => U)(implicit typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule[TermType0, TypeType0, U] {
  override def run(implicit typeSystem: TypeSystemT): Result = rule.run.right.map(t => t.copy(t = f(t.t)))
}