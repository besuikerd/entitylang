package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.analysis.types.multiplicity._
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.typesystem.entitylang.typingrule._
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{AlternativeTypingRule, TypingRule}
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.{Apply2, If3, MemberAccess2, Ref1, Null0}
import org.metaborg.entitylang.lang.ast.MExpression.SLiteral._
import org.metaborg.scalaterms.HasOrigin

object ExpressionTypeSystem extends FancyTypeSystem[SExp, Type]{
  type FunctionN = TypeSystem[SExp, Type] => HasOrigin => Seq[SExp] => TypingRule.Aux[SExp, Type, Type]
  type Function1 = TypeSystem[SExp, Type] => SExp => TypingRule.Aux[SExp, Type, Type]

  def function1(inner: Function1): FunctionN = implicit typeSystem => origin => expressions =>
    if(expressions.isEmpty)
      typeRule.fail(origin, "Expected at least 1 argument")
    else
      inner(typeSystem)(expressions.head)

  val manyNumToNum: Function1 = implicit typeSystem => e1 => for{
    t1 <- boundedNumeric(e1, zeroToMany)
  } yield t1.baseType.one

  val manyBoolToBool: Function1 = implicit typeSystem => e1 => for{
    t1 <- multiplicityType[BooleanType](e1).flatMap{t => upperBounded(e1, t, zeroToMany)}
  } yield boolean.one

  val manyAnyToInt: Function1 = implicit typesystem => e1 => for{
    t1 <- multiplicityType[BaseType](e1).flatMap{t => upperBounded(e1, t, zeroToMany)}
  } yield int.one

  val functions: Map[String, FunctionN] = Map(
    "min" -> function1(manyNumToNum),
    "max" -> function1(manyNumToNum),
    "avg" -> function1(manyNumToNum),
    "sum" -> function1(manyNumToNum),
    "count" -> function1(manyAnyToInt),
    "conj" -> function1(manyBoolToBool),
    "disj" -> function1(manyBoolToBool)
  )

  rule[If3](implicit typeSystem => {
    case If3(e1, e2, e3, _) =>
      for{
        t1 <- e1.infer.ofType(boolean.one)
        (t2, t3) <- typeRule.all(
          multiplicityType[BaseType](e2),
          multiplicityType[BaseType](e3)
        )
        t4 <- lub(e2, t2, t3)
      } yield t4
  })

  rule[True0](implicit typeSystem => t => typeRule.success(boolean.one))
  rule[False0](implicit typeSystem => f => typeRule.success(boolean.one))
  rule[Int1](implicit typeSystem => i => typeRule.success(int.one))
  rule[String1](implicit typeSystem => s => typeRule.success(string.one))
  rule[Float1](implicit typeSystem => f => typeRule.success(float.one))
  rule[Null0](implicit typeSystem => f => typeRule.success(any.zero))

  partialRule(implicit typeSystem => {
    case UnExp(op, e1) =>
      op match {
        case Not =>
          for {
            t1 <- e1.infer.ofType(boolean.one)
          } yield t1
      }
  })

  partialRule(implicit typeSystem => {
    case term @ BinExp(op, e1, e2) => op match {
      case Add => {
        val caseAdd = for {
          (t1, t2) <-
          typeRule.all(
            boundedNumeric(e1, zeroToOne),
            boundedNumeric(e2, zeroToOne)
          )
          t3 <- lub(e2, t1, t2)
        } yield t3
        val caseConcat = for{
          (t1, t2) <-
          typeRule.all(
            boundedMultiplicityType[StringType](e1, zeroToOne),
            boundedMultiplicityType[BaseType](e2, zeroToOne)
          )
          m <- lubMultiplicity(e2, t1.multiplicity, t2.multiplicity)
        } yield string withMultiplicity m

        typeRule.alternative(
          caseAdd,
          caseConcat
        )
      }

      case NumericOperator(op) =>
        for {
          (t1, t2) <-
            typeRule.all(
              boundedNumeric(e1, zeroToOne),
              boundedNumeric(e2, zeroToOne)
            )
            t3 <- lub(e2, t1, t2)
        } yield t3

      case Mod =>
        for{
          (t1, t2) <- typeRule.all(
            boundedMultiplicityType(int, e1, zeroToOne),
            boundedMultiplicityType(int, e2, zeroToOne)
          )
          m <- lubMultiplicity(e2, t1.multiplicity, t2.multiplicity)
        } yield int withMultiplicity m

      case CompareOperator(_) =>
        for{
          (t1, t2) <- typeRule.all(
            boundedNumeric(e1, zeroToOne),
            boundedNumeric(e2, zeroToOne)
          )
          m <- lubMultiplicity(e2, t1.multiplicity, t2.multiplicity)
        } yield boolean withMultiplicity m

      case Equal =>
        for{
          (t1, t2) <- typeRule.all(
            boundedMultiplicityType[BaseType](e1, zeroToOne),
            boundedMultiplicityType[BaseType](e2, zeroToOne)
          )
          t3 <- lub(e2, t1, t2)
        } yield boolean withMultiplicity t3.multiplicity

      case Inequal =>
        for{
          (t1, t2) <- typeRule.all(
            boundedMultiplicityType[BaseType](e1, zeroToOne),
            boundedMultiplicityType[BaseType](e2, zeroToOne)
          )
          t3 <- lub(e2, t1, t2)
        } yield boolean withMultiplicity t3.multiplicity

      case And =>
        for{
          (t1, t2) <- typeRule.all(
            boundedMultiplicityType(boolean, e1, zeroToOne),
            boundedMultiplicityType(boolean, e2, zeroToOne)
          )
          t3 <- lub(e2, t1, t2)
        } yield t3

      case Or =>
        for{
          (t1, t2) <- typeRule.all(
            boundedMultiplicityType(boolean, e1, zeroToOne),
            boundedMultiplicityType(boolean, e2, zeroToOne)
          )
          t3 <- lub(e2, t1, t2)
        } yield t3

      case ChoiceLeft =>
      for {
        (t1, t2) <-
          typeRule.all(
            maybeEmpty[BaseType](e1),
            multiplicityType[BaseType](e2)
          )
        t3 <- lub(e2, t1, t2)
      } yield t3

      //      case Merge =>
      case _ => typeRule.fail(term, op + " not implemented yet")
    }
  })

  rule[Apply2](implicit typeSystem => {
    case a @ Apply2(e1, args, _) =>
      e1 match{
        case Ref1(id, _) => functions.get(id.string) match{
          case Some(f) => f.apply(typeSystem)(e1)(args.value)
          case None => typeRule.fail(e1, "function not found: " + id.string)
        }
        case _ => typeRule.fail(e1, "expected an identifier")
      }
  })

  rule[Ref1](implicit typeSystem => {
    case r @ Ref1(id1, _) =>
      for {
        t1 <- typeRule.fromTypeEnvironment(r, id1.string)
      } yield t1
  })

  rule[MemberAccess2](implicit typeSystem => {
    case m @ MemberAccess2(e, id, _) =>
      for {
        MultiplicityType(EntityType(name), multiplicity) <- entity(e)
        MultiplicityType(baseType, multiplicity2) <- typeRule.fromTypeEnvironment(id, s"$name.${id.string}").ofType[MultiplicityType[BaseType]]
        t2 <- multiplicity.tryCompare(multiplicity2) match {
          case Some(n) => typeRule.success[Type](MultiplicityType(baseType, if (n > 0) multiplicity2 else multiplicity))
          case None => typeRule.fail[Type](id, "field can not have a valid multiplicity")
        }
      } yield t2
  })
}
