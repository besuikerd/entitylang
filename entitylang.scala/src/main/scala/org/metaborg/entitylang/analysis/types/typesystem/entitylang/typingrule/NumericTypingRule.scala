package org.metaborg.entitylang.analysis.types.typesystem.entitylang.typingrule

import org.metaborg.entitylang.analysis.types.{NumericType}
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.lang.ast.MExpression.SExp

class NumericTypingRule(e: SExp) extends EntityLangTypingRule{
  override type T = NumericType
  override def run(implicit typeSystem: TypeSystemT): TypingResult =
    e.infer.run.right.flatMap {
      case n: NumericType => Right(n)
      case otherwise => typeError(e, "Expected numeric type, got " + otherwise)
    }
}

object NumericTypingRule{
  import org.metaborg.entitylang.analysis.types.typesystem._
  import org.metaborg.entitylang.analysis.types._
  def numeric(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]) = e.infer.ofType[NumericType]
}