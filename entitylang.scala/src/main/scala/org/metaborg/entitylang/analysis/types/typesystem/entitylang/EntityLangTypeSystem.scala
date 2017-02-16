package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.typesystem.{TopLevelTypingRule, TypeSystem, matching, success}
import org.metaborg.entitylang.analysis.types.{BooleanType, FunctionType, IntType, NumericType, Type, boolean, float, int, string}
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.{Apply2, If3, Ref1}
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
    binExp,
    apply2,
    ref1
  ).withBindings(Map(


    "f" -> (int ~> string)
  ))

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
        } yield lub(t1, t2)

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

      case _ => fail(term, op + " not implemented yet")
//      case Merge =>
//      case ChoiceLeft =>
    }
  }

  def apply2: Rule = {
    case Apply2(e1, args, _) =>
      for {
        FunctionType(from, to) <- e1.ofType[FunctionType]("Function")
        t1 <- args.value.head.infer
        _ <- if(t1 != from) rule.mismatchedType(e1, from, t1) else success(t1)
      } yield to
  }

  def ref1: Rule = {
    case r @ Ref1(id1, _) =>
      for {
        t1 <- rule.fromTypeEnvironment(r, id1.string)
      } yield t1
  }
}
