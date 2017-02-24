package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.multiplicity._
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{TermTypingRule, TypingRule}
import org.metaborg.entitylang.analysis.types.{BaseType, EntityType, MultiplicityType, NumericType, Type, typesystem}
import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.scalaterms.HasOrigin

import scala.reflect.{ClassTag, classTag}

package object typingrule {

  def multiplicityType[T <: BaseType: ClassTag](e: SExp, humanReadbleName: String)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] = multiplicityType[T](e, Some(humanReadbleName))
  def multiplicityType[T <: BaseType: ClassTag](e: SExp, humanReadbleName: Option[String] = None)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] = {
    val cls = classTag[T].runtimeClass.asInstanceOf[Class[T]]
    val x = for {
      MultiplicityType(baseType, multiplicity) <- e.infer.ofType[MultiplicityType[BaseType]]("MultiplicityType")
      t2 <-
      if(cls.isInstance(baseType))
        typeRule.success(MultiplicityType(cls.cast(baseType), multiplicity))
      else
        typeRule.fail[MultiplicityType[T]](e, s"Expected base type ${humanReadbleName.getOrElse(cls.getName)}, got: ${Type.ppBaseType(baseType)}" )
    } yield t2
    x.bindTerm(e)
  }

  def multiplicityType[T <: BaseType](baseType: T, e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] = {
    val x = for {
      MultiplicityType(baseType2, multiplicity) <- e.infer.ofType[MultiplicityType[BaseType]]("MultiplicityType")
      t2 <-
        if(baseType == baseType2)
          typeRule.success(MultiplicityType(baseType, multiplicity))
        else
          typeRule.fail[MultiplicityType[T]](e, s"Expected base type ${Type.ppBaseType(baseType)}, got: ${Type.ppBaseType(baseType2)}" )
    } yield t2
    x.bindTerm(e)
  }
  def boundedMultiplicityType[T <: BaseType](baseType: T, e: SExp, upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]) =
    multiplicityType(baseType, e).flatMap{t => upperBounded(e, t, upperBound)}

  def boundedMultiplicityType[T <: BaseType: ClassTag](e: SExp, upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]) =
    multiplicityType[T](e).flatMap{t => upperBounded(e, t, upperBound)}

  def lub(o: HasOrigin, t1: MultiplicityType[BaseType], t2: MultiplicityType[BaseType])(implicit typeSystem: TypeSystem[SExp, Type], ord: Ordering[NumericType]): TypingRule.Aux[SExp, Type, MultiplicityType[BaseType]] = {
    for {
      baseType <- (t1.baseType, t2.baseType) match{
        case (n1: NumericType, n2: NumericType) => typeRule.success[BaseType](ord.max(n1, n2))
        case (b1, b2) if b1 == b2 => typeRule.success[BaseType](b1)
        case (b1, b2) => typeRule.fail[BaseType](o, s"incompatible base types: ${Type.ppBaseType(b1)} != ${Type.ppBaseType(b2)}")
      }
      multiplicity <- lubMultiplicity(o, t1.multiplicity, t2.multiplicity)
    } yield MultiplicityType(baseType, multiplicity)
  }

  def lubMultiplicity(o: HasOrigin, m1: MultiplicityBounds, m2: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityBounds] =
    (m1.tryCompare(m2) match{
      case Some(i) => typeRule(typeSystem).success(if(i > 0) m2 else m1)
      case None => typeRule.fail[MultiplicityBounds](o, s"incompatible multiplicity bounds: $m1 <-> $m2")
    }).bindTerm(o)

  def maybeEmpty[T <: BaseType : ClassTag](e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] =
    multiplicityType[T](e, "Any").filter(_.multiplicity.lowerBound == Multiplicity.zero , t => "type should be able to inhabit no instances")

  def maybeEmpty[T <: BaseType](baseType: T, e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[BaseType]] =
    multiplicityType[BaseType](baseType, e).filter(_.multiplicity.lowerBound == Multiplicity.zero , t => "type should be able to inhabit no instances")

  def numeric(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[NumericType]] =
    multiplicityType[NumericType](e, "Number")

  def boundedNumeric(e: SExp, upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[NumericType]] =
    numeric(e).flatMap{t => upperBounded(e, t, upperBound)}

  def upperBounded[T <: BaseType](origin: HasOrigin, t: MultiplicityType[T], upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] =
    typeRule.success(origin, t).filter(
      _.multiplicity <= upperBound, t => s"invalid bounds, expected at most $upperBound, got ${t.multiplicity}"
    )

  def entity(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[EntityType]] =
    multiplicityType[EntityType](e, "Entity")
}
