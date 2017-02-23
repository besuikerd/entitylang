package org.metaborg.entitylang.analysis.types.typesystem.entitylang

import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds
import org.metaborg.entitylang.analysis.types.typesystem._
import org.metaborg.entitylang.analysis.types.typesystem.typingrule.{TermTypingRule, TypingRule}
import org.metaborg.entitylang.analysis.types.{BaseType, EntityType, MultiplicityType, NumericType, Type}
import org.metaborg.entitylang.lang.ast.MExpression.SExp

import scala.reflect.{ClassTag, classTag}

package object typingrule {

  def multiplicityType[T <: BaseType: ClassTag](e: SExp, humanReadbleName: String)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] = multiplicityType[T](e, Some(humanReadbleName))
  def multiplicityType[T <: BaseType: ClassTag](e: SExp, humanReadbleName: Option[String] = None)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[T]] = {
    val cls = classTag[T].runtimeClass.asInstanceOf[Class[T]]
    val x = for {
      MultiplicityType(baseType, multiplicity) <- e.infer.ofType[MultiplicityType[BaseType]]("MultiplicityType")
      t2 <-
        if(cls.isInstance(baseType))
          rule.success(MultiplicityType(cls.cast(baseType), multiplicity))
        else
          rule.fail[MultiplicityType[T]](e, s"Expected base type ${humanReadbleName.getOrElse(cls.getName)}, got: ${Type.ppBaseType(baseType)}" )
    } yield t2
    x.bindTerm(e)
  }

  def numeric(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[NumericType]] =
    multiplicityType[NumericType](e, "Number")

  def numeric(e: SExp, upperBound: MultiplicityBounds)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[NumericType]] =
    numeric(e).flatMap{ case mt @ MultiplicityType(t, m) =>
      if(m <= upperBound)
        rule.success(mt)
      else
        rule.fail[MultiplicityType[NumericType]](e, s"invalid bounds, expected at most $upperBound")
    }.bindTerm(e)

  def entity(e: SExp)(implicit typeSystem: TypeSystem[SExp, Type]): TermTypingRule[SExp, Type, MultiplicityType[EntityType]] =
    multiplicityType[EntityType](e, "Entity")
}
