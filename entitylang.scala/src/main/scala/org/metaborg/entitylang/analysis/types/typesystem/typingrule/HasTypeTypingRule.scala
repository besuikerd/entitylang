package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.entitylang.analysis.types.typesystem.TypeSystem
import org.metaborg.scalaterms.HasOrigin

class HasTypeTypingRule[TermType <: HasOrigin, TypeType, Term <: TermType, T <: TypeType](val term: Term, t: T)(implicit typeSystem: TypeSystem[TermType, TypeType]) extends TermTypingRule[TermType, TypeType, T]{
  override def run(implicit typeSystem: TypeSystemT): Result = {
    typeSystem.infer(term).right.flatMap(result =>
      if(t == result.t)
        Right(result.copy(t = t))
      else
        typeError(term, s"Expected type: $t, got: ${result.t}")
    )
  }
}
