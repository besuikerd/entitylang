package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.typesystem.typingrule.TypingRule
import org.metaborg.entitylang.analysis.types.{NumericType, Type}
import org.metaborg.entitylang.lang.ast.MExpression.SExp

package object typingrule {
  def numeric(e: SExp): TypingRule.Aux[SExp, Type, NumericType] = new NumericTypingRule(e)
}
