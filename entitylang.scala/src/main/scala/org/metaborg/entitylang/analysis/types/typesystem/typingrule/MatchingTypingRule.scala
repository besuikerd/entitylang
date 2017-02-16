package org.metaborg.entitylang.analysis.types.typesystem.typingrule

class MatchingTypingRule[TermType0, TypeType0](terms: TermType0*) extends TypingRule {
  override type TermType = TermType0
  override type TypeType = TypeType0
  override type T = TypeType0

  override def run(implicit typeSystem: TypeSystemT) : TypingResult = {
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