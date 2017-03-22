package org.metaborg.entitylang.analysis.types.typesystem.typingrule

import org.metaborg.scalaterms.HasOrigin

class MatchingTypingRule[TermType <: HasOrigin, TypeType](terms: TermType*) extends TypingRule[TermType, TypeType, TypeType] {
  override def run(implicit typeSystem: TypeSystemT) : Result = {
    if(terms.isEmpty) {
      internalError("Empty set of matching rules")
    } else if(terms.size == 1){
      typeSystem.infer(terms.head)
    } else{
      terms.tail.foldLeft(typeSystem.infer(terms.head)) {
        case (Left(s), _) => Left(s)
        case (Right(t1), e) => typeSystem.infer(e) match{
          case Right(t2) => if (t1 == t2) Right(t1) else typeError(e, s"Expected type: $t1, got: $t2")
          case otherwise => otherwise
        }
      }
    }
  }
}