package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.analysis.types.multiplicity.MultiplicityBounds

sealed trait Type

sealed trait BaseType

object BaseType{


  val partialOrdering: PartialOrdering[BaseType] = new PartialOrdering[BaseType]{
    override def tryCompare(x: BaseType, y: BaseType): Option[Int] =
      (x, y) match{
        case (x: NumericType, y: NumericType) =>
          NumericType.numericOrdering.tryCompare(x, y)
        case (x: AnyType, y: AnyType) => Some(0)
        case (x: AnyType, y) => Some(1)
        case (x, y: AnyType) => Some(-1)
        case (x, y) if x == y => Some(0)
        case _ => None
      }

    override def lteq(x: BaseType, y: BaseType): Boolean = tryCompare(x, y).exists(_ <= 0)
  }

}

case class StringType() extends BaseType
case class BooleanType() extends BaseType
case class AnyType() extends BaseType

sealed trait NumericType extends BaseType
case class LongType() extends NumericType
case class IntType() extends NumericType
case class FloatType() extends NumericType


object NumericType{
  implicit val numericOrdering: Ordering[NumericType] = new Ordering[NumericType]{
    override def compare(x: NumericType, y: NumericType): Int = orderNum(x) - orderNum(y)

    def orderNum(t: NumericType) = t match {
      case IntType() => 0
      case LongType() => 1
      case FloatType() => 2
    }
  }
}

case class EntityType(name: String) extends BaseType

case class MultiplicityType[+T <: BaseType](baseType: T, multiplicity: MultiplicityBounds) extends Type

case class TopType() extends Type
case class FunctionType(t1: Type, t2: Type) extends Type

object Type{
  def ppType(t: Type): String = t match {
    case MultiplicityType(baseType, multiplicity) => s"${ppBaseType(baseType)}${multiplicity}"
    case TopType() => "⊤"
    case FunctionType(t1, t2) => s"${ppType(t1)} -> ${ppType(t2)}"
  }

  def ppBaseType(t: BaseType): String = t match {
    case StringType() => "string"
    case BooleanType() => "boolean"
    case IntType() => "int"
    case LongType() => "long"
    case FloatType() => "float"
    case AnyType() => "any"
    case EntityType(name) => name
  }
}