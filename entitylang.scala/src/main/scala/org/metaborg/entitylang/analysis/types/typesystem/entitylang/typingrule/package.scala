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
        typeRule.success(e, MultiplicityType(cls.cast(baseType), multiplicity))
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
          typeRule.success(e, MultiplicityType(baseType, multiplicity))
        else
          typeRule.fail[MultiplicityType[T]](e, s"Expected base type ${Type.ppBaseType(baseType)}, got: ${Type.ppBaseType(baseType2)}" )
    } yield t2
    x.bindTerm(e)
  }
  def boundedMultiplicityType[T <: BaseType](baseType: T, e: SExp, upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]) =
    multiplicityType(baseType, e).flatMap{t => upperBounded(e, t, upperBound)}

  def boundedMultiplicityType[T <: BaseType: ClassTag](e: SExp, upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]) =
    multiplicityType[T](e).flatMap{t => upperBounded(e, t, upperBound)}

  def lub(o: HasOrigin, t1: MultiplicityType[BaseType], t2: MultiplicityType[BaseType])(implicit typeSystem: TypeSystem[SExp, Type], ord: Ordering[NumericType]): TypingRule[SExp, Type, MultiplicityType[BaseType]] = {
    for {
      baseType <- lubBaseType(o, t1.baseType, t2.baseType)
      multiplicity <- lubMultiplicity(t1.multiplicity, t2.multiplicity)
    } yield MultiplicityType(baseType, multiplicity)
  }

  def merge(o: HasOrigin, t1: MultiplicityType[BaseType], t2: MultiplicityType[BaseType])(implicit typeSystem: TypeSystem[SExp, Type]): TypingRule[SExp, Type, MultiplicityType[BaseType]] = {
    for{
      baseType <- lubBaseType(o, t1.baseType, t2.baseType)
      multiplicity = MultiplicityBounds.merge(t1.multiplicity, t2.multiplicity)
    } yield MultiplicityType(baseType, multiplicity)
  }

  def lubBaseType(o: HasOrigin, t1: BaseType, t2: BaseType)(implicit typeSystem: TypeSystem[SExp, Type]): TypingRule[SExp, Type, BaseType] =
    if(BaseType.partialOrdering.gteq(t1, t2))
      typeRule.success[BaseType](t1)
    else if(BaseType.partialOrdering.gteq(t2, t1))
      typeRule.success[BaseType](t2)
    else
      typeRule.fail[BaseType](o, s"incompatible base types: ${Type.ppBaseType(t1)} <-> ${Type.ppBaseType(t2)}")

  def lubMultiplicity(m1: MultiplicityBounds, m2: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]): TypingRule[SExp, Type, MultiplicityBounds] =
    typeRule.success(MultiplicityBounds.lub(m1, m2))

  def maybeEmpty[T <: BaseType : ClassTag](e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] =
    multiplicityType[T](e, "Any")
      .filter(_.multiplicity.lowerBound == Multiplicity.zero , t => "type should be able to inhabit no instances")
      .map(t => MultiplicityType(t.baseType, MultiplicityBounds.nonZero(t.multiplicity)))

  def maybeEmpty[T <: BaseType](baseType: T, e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[BaseType]] =
    multiplicityType[BaseType](baseType, e).filter(_.multiplicity.lowerBound == Multiplicity.zero , t => "type should be able to inhabit no instances")

  def numeric(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[NumericType]] =
    multiplicityType[NumericType](e, "Number")

  def boundedNumeric(e: SExp, upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[NumericType]] =
    numeric(e).flatMap{t => upperBounded(e, t, upperBound)}

  def upperBounded[T <: BaseType](origin: HasOrigin, t: MultiplicityType[T], upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] =
    typeRule.success(t).bindTerm(origin).filter(
      _.multiplicity <= upperBound, t => s"invalid bounds, expected at most $upperBound, got ${t.multiplicity}"
    )

  def entity(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[EntityType]] =
    multiplicityType[EntityType](e, "Entity")
}
