package org.metaborg.entitylang.desugar

import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.Not1

sealed trait UnaryOperand
case object Not extends UnaryOperand

object UnExp{
  def unapply(e: SExp): Option[(UnaryOperand, SExp)] = e match{
    case Not1(exp1, origin) => Some((Not, exp1))
    case _ => None
  }
}
