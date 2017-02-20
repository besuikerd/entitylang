package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{TermTypingRule, TypingRule}
import org.metaborg.entitylang.analysis.types.{NumericType, Type}
import org.metaborg.entitylang.lang.ast.MExpression.SExp

package object typingrule {
  def numeric(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, NumericType] = e.infer.ofType[NumericType]("Number")
}
