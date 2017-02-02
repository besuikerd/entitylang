package org.metaborg.entitylang.desugar

import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp._


sealed trait BinaryOperator

case object Add extends BinaryOperator
case object Sub extends BinaryOperator
case object Mul extends BinaryOperator
case object Div extends BinaryOperator
case object Mod extends BinaryOperator
case object LessThan extends BinaryOperator
case object LessThanEqual extends BinaryOperator
case object GreaterThan extends BinaryOperator
case object GreaterThanEqual extends BinaryOperator
case object Equal extends BinaryOperator
case object Inequal extends BinaryOperator
case object And extends BinaryOperator
case object Or extends BinaryOperator
case object Merge extends BinaryOperator
case object ChoiceLeft extends BinaryOperator

object BinExp{
  def unapply(e: SExp): Option[(BinaryOperator, SExp, SExp)] = e match {
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
    case Merge2(exp1, exp2, origin) => Some((Merge, exp1, exp2))
    case ChoiceLeft2(exp1, exp2, origin) => Some((ChoiceLeft, exp1, exp2))
    case _ => None
  }
}
