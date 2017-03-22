package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

class FilteredTypingRule[TermType <: HasOrigin, TypeType, T](rule: TypingRule[TermType, TypeType, T], f: T => Boolean) extends TypingRule[TermType, TypeType, T] {
  override def run(implicit typeSystem: TypeSystem[TermType, TypeType]): Result =
    rule.run.right.flatMap{ t => if(f(t.t)) Right(t) else internalError("Match failed on term")}
}