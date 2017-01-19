package org.metaborg.entitylang.desugar

import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp._


sealed trait BinaryOperand

case object Add extends BinaryOperand
case object Sub extends BinaryOperand
case object Mul extends BinaryOperand
case object Div extends BinaryOperand
case object Mod extends BinaryOperand
case object LessThan extends BinaryOperand
case object LessThanEqual extends BinaryOperand
case object GreaterThan extends BinaryOperand
case object GreaterThanEqual extends BinaryOperand
case object Equal extends BinaryOperand
case object Inequal extends BinaryOperand
case object And extends BinaryOperand
case object Or extends BinaryOperand

object BinExp{
  def unapply(e: SExp): Option[(BinaryOperand, SExp, SExp)] = e match {
    case Multiplication2(exp1, exp2, origin) => Some((Mul, exp1, exp2))
    case Division2(exp1, exp2, origin) => Some((Div, exp1, exp2))
    case Modulo2(exp1, exp2, origin) => Some((Mod, exp1, exp2))
    case Addition2(exp1, exp2, origin) => Some((Add, exp1, exp2))
    case Subtraction2(exp1, exp2, origin) => Some((Sub, exp1, exp2))
    case LessThan2(exp1, exp2, origin) => Some((LessThan, exp1, exp2))
    case LessThanEqual2(exp1, exp2, origin) => Some((LessThanEqual, exp1, exp2))
    case GreaterThan2(exp1, exp2, origin) => Some((GreaterThan, exp1, exp2))
    case GreaterThanEqual2(exp1, exp2, origin) => Some((GreaterThanEqual, exp1, exp2))
    case Equal2(exp1, exp2, origin) => Some((Equal, exp1, exp2))
    case Inequal2(exp1, exp2, origin) => Some((Inequal, exp1, exp2))
    case And2(exp1, exp2, origin) => Some((And, exp1, exp2))
    case Or2(exp1, exp2, origin) => Some((Or, exp1, exp2))
    case _ => None
  }
}
