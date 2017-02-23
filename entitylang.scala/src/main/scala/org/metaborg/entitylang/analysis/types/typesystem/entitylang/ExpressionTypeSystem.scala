package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.multiplicity.Multiplicity
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.multiplicity._
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.typingrule._
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{TermTypingRule, TypingRule}
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.{Apply2, If3, MemberAccess2, Ref1}
import org.metaborg.entitylang.lang.ast.MExpression.SLiteral._
import org.metaborg.scalaterms.HasOrigin

object ExpressionTypeSystem {
//  val builtin = Map(
//    "min" -> (int ~>: int),
//    "max" -> (int ~>: int),
//    "avg" -> (int ~>: int),
//    "sum" -> (int ~>: int),
//    "conj" -> (boolean ~>: boolean),
//    "disj" -> (boolean ~>: boolean),
//  )

  type FunctionN = TypeSystem[SExp, Type] => HasOrigin => Seq[SExp] => TypingRule.Aux[SExp, Type, Type]
  type Function1 = TypeSystem[SExp, Type] => SExp => TypingRule.Aux[SExp, Type, Type]

  def function1(inner: Function1): FunctionN = implicit typeSystem => origin => expressions =>
    if(expressions.isEmpty)
      rule.fail(origin, "Expected at least 1 argument")
    else
      inner(typeSystem)(expressions.head)

  val manyNumToNum: Function1 = implicit typeSystem => e1 => for{
    t1 <- numeric(e1, zeroToMany)
  } yield t1.baseType.one

  val manyBoolToBool: Function1 = implicit typeSystem => e1 => for{
    t1 <- multiplicityType[BooleanType](e1).flatMap{t => upperBounded(e1, t, zeroToMany)}
  } yield boolean.one

  val functions: Map[String, FunctionN] = Map(
    "min" -> function1(manyNumToNum),
    "max" -> function1(manyNumToNum),
    "avg" -> function1(manyNumToNum),
    "sum" -> function1(manyNumToNum),
    "conj" -> function1(manyBoolToBool),
    "disj" -> function1(manyBoolToBool)
  )

  val typeSystem = TypeSystem(
    if3,
    true0,
    false0,
    int1,
    string1,
    float1,
    unExp,
    binExp,
    apply2,
    ref1,
    memberAccess2
  )



  implicit val ppType: (Type) => String = Type.ppType

  type Rule = TopLevelTypingRule[SExp, Type]

  def if3: Rule = implicit typeSystem => {
    case If3(e1, e2, e3, _) =>
      for{
        t1 <- e1.infer.ofType(boolean.one)(typeSystem, ppType)
        t2 <- matching(e2, e3)
      } yield t2
  }

  def true0: Rule = implicit typeSystem => {
    case t @ True0(_) => rule.success(boolean.one)
  }

  def false0: Rule = implicit typeSystem => {
    case False0(_) => rule.success(boolean.one)
  }

  def int1: Rule = implicit typeSystem =>{
    case Int1(_, _) => rule.success(int.one)
  }

  def string1: Rule = implicit typeSystem => {
    case String1(_, _) => rule.success(string.one)
  }

  def float1: Rule = implicit typeSystem => {
    case Float1(_, _) => rule.success(float.one)
  }

  def unExp: Rule = implicit typeSystem => {
    case UnExp(op, e1) =>
      op match {
        case Not =>
          for {
            t1 <- e1.infer.ofType(boolean.one)
          } yield t1
      }
  }

  def binExp: Rule = implicit typeSystem => {
    case term @ BinExp(op, e1, e2) => op match {
      case NumericOperator(_) =>
        for {
          (MultiplicityType(t1, m1), MultiplicityType(t2, m2)) <-
            rule.all(
              numeric(e1, zeroToOne),
              numeric(e2, zeroToOne)
            )
        } yield MultiplicityType(lub(t1, t2), if(m1 > m2) m1 else m2)

      case Mod =>
        for{
          t1 <- e1.infer.ofType(int.one)
          t2 <- e2.infer.ofType(int.one)
        } yield int.one

      case CompareOperator(_) =>
        for{
          t1 <- numeric(e1)
          t2 <- numeric(e2)
        } yield boolean.one

      case Equal =>
        for {
          t1 <- e1.infer
          t2 <- e2.infer
        } yield boolean.one

      case Inequal =>
        for {
          t1 <- e1.infer
          t2 <- e2.infer
        } yield boolean.one
      case And =>
        for {
          t1 <- e1.infer.ofType(boolean.one)
          t2 <- e2.infer.ofType(boolean.one)
        } yield boolean.one

      case Or =>
        for {
          t1 <- e1.infer.ofType(boolean.one)
          t2 <- e2.infer.ofType(boolean.one)
        } yield boolean.one

      case _ => rule.fail(term, op + " not implemented yet")
//      case Merge =>
//      case ChoiceLeft =>
    }
  }

  def apply2: Rule = implicit typeSystem => {
    case a @ Apply2(e1, args, _) =>
      e1 match{
        case Ref1(id, _) => functions.get(id.string) match{
          case Some(f) => f.apply(typeSystem)(e1)(args.value)
          case None => rule.fail(e1, "function not found: " + id.string)
        }
        case _ => rule.fail(e1, "expected an identifier")
      }
  }

//  def apply2: Rule = implicit typeSystem => {
//    case a@Apply2(e1, args, _) =>
//      args.value.foldLeft[TermTypingRule[SExp, Type, Type]](e1.infer) {
//        case (acc, current) =>
//          val x = for {
//            f <- acc.ofType[FunctionType]("FunctionType")
//            t1 <- current.infer
//            t2 <- if (t1 != f.t1) rule.mismatchedType(current, f.t1, t1) else rule.success(f.t2)
//          } yield t2
//          x.bindTerm(current)
//      }
//    }

  def ref1: Rule = implicit typeSystem => {
    case r @ Ref1(id1, _) =>
      for {
        t1 <- rule.fromTypeEnvironment(r, id1.string)
      } yield t1
  }

  def memberAccess2: Rule = implicit typeSystem => {
    case m @ MemberAccess2(e, id, _) =>
      for {
        MultiplicityType(EntityType(name), multiplicity) <- entity(e)
        MultiplicityType(baseType, multiplicity2) <- rule.fromTypeEnvironment(id, s"$name.${id.string}").ofType[MultiplicityType[BaseType]]
        t2 <- multiplicity.tryCompare(multiplicity2) match {
          case Some(n) => rule.success[Type](MultiplicityType(baseType, if (n > 0) multiplicity2 else multiplicity))
          case None => rule.fail[Type](id, "field can not have a valid multiplicity")
        }
      } yield t2
  }
}
