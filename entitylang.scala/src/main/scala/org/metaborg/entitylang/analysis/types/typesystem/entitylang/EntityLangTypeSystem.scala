package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.typesystem.{TopLevelTypingRule, TypeSystem, matching, success}
import org.metaborg.entitylang.analysis.types.{BooleanType, IntType, NumericType, Type, boolean, float, int, string}
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
import org.metaborg.entitylang.lang.ast.MExpression.SLiteral._
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.typingrule._

object EntityLangTypeSystem {
  implicit val typeSystem = TypeSystem(
    if3,
    true0,
    false0,
    int1,
    string1,
    float1,
    unExp,
    binExp
  )

  type Rule = TopLevelTypingRule[SExp, Type]

  def if3: Rule = {
    case If3(e1, e2, e3, _) =>
      for{
        t1 <- e1.ofType[BooleanType]
        t2 <- matching(e2, e3)
      } yield t2
  }

  def true0: Rule = {
    case t @ True0(_) => success(boolean)
  }

  def false0: Rule = {
    case False0(_) => success(boolean)
  }

  def int1: Rule = {
    case Int1(_, _) => success(int)
  }

  def string1: Rule = {
    case String1(_, _) => success(string)
  }

  def float1: Rule = {
    case Float1(_, _) => success(float)
  }

  def unExp: Rule = {
    case UnExp(op, e1) =>
      op match {
        case Not =>
          for {
            t1 <- e1.ofType[BooleanType]
          } yield t1
      }
  }

  def binExp: Rule = {
    case term @ BinExp(op, e1, e2) => op match {
      case NumericOperator(_) =>
        for {
          t1 <- numeric(e1)
          t2 <- numeric(e2)
          t3 <- rule.success(lub(t1, t2))
        } yield t3

      case Mod =>
        for{
          t1 <- e1.ofType[IntType]
          t2 <- e2.ofType[IntType]
        } yield int

      case CompareOperator(_) =>
        for{
          t1 <- numeric(e1)
          t2 <- numeric(e2)
        } yield boolean


      case Equal =>
        for {
          t1 <- e1.infer
          t2 <- e2.infer
        } yield boolean

      case Inequal =>
        for {
          t1 <- e1.infer
          t2 <- e2.infer
        } yield boolean

      case And =>
        for {
          t1 <- e1.ofType[BooleanType]
          t2 <- e2.ofType[BooleanType]
        } yield boolean

      case Or =>
        for {
          t1 <- e1.ofType[BooleanType]
          t2 <- e2.ofType[BooleanType]
        } yield boolean

      case _ => fail(term, op + "Not implemented yet")
//      case Merge =>
//      case ChoiceLeft =>
    }
  }
}
