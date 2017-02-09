package org.metaborg.entitylang.analysis

package object types {

  sealed trait Type{
    def ~>(t2: Type) = FunctionType(this, t2)
    def ~>:(t2: Type) = t2 ~> this

    def |(t2: Type) = LeastUpperBound(this, t2)
  }

  def ppType(t: Type): String = t match {
    case StringType() => "string"
    case BooleanType() => "boolean"
    case IntType() => "int"
    case FloatType() => "float"
    case EntityType(name) => name
    case TopType() => "Any"
    case FunctionType(t1, t2) => s"${ppType(t1)} -> ${ppType(t2)}"
    case LeastUpperBound(t1, t2) => s"(${ppType(t1)} | ${ppType(t2)})"
    case TypeVariable(name) => "$" + name
    case TypeRestriction(relation, variable, tpe) => s"${ppTypeRelation(relation)} $variable => ${ppType(tpe)}"
  }

  def ppTypeRelation(t: TypeRelation): String = t match {
    case Covariant(t1, t2) => s"${ppTypeRelation(t1)} <= ${ppTypeRelation(t2)}"
    case Alias(name, _) => name
    case Alternative(t1, t2) => s"${ppTypeRelation(t1)} | ${ppTypeRelation(t2)}"
    case Invariant(t) => ppType(t)
  }

  case class StringType() extends Type
  case class BooleanType() extends Type
  case class IntType() extends Type
  case class FloatType() extends Type
  case class EntityType(name: String) extends Type
  case class TopType() extends Type
  case class FunctionType(t1: Type, t2: Type) extends Type

  case class LeastUpperBound(t1: Type, t2: Type) extends Type
  case class TypeVariable(name: String) extends Type
  case class TypeRestriction(relation: TypeRelation, variable: String, tpe: Type) extends Type


  sealed trait TypeRelation {
    def <=(t2: TypeRelation) = Covariant(this, t2)
  }
  case class Alias(name: String, relation: TypeRelation) extends TypeRelation
  case class Covariant(t1: TypeRelation, t2: TypeRelation) extends TypeRelation
  case class Alternative(t1: TypeRelation, t2: TypeRelation) extends TypeRelation
  case class Invariant(t: Type) extends TypeRelation

  implicit def typeToInvariantRelation(t: Type): TypeRelation = Invariant(t)

  implicit class RichType(val t: Type) extends AnyVal{

    def <=(t2: Type) = Covariant(t, t2)
    def <+(t2: TypeRelation) = Alternative(t, t2)

    def ~>(t2: Type) = FunctionType(t, t2)

    def =>:(partialTypeRestriction: PartialTypeRestriction) = TypeRestriction(partialTypeRestriction.relation, partialTypeRestriction.variable, t)
  }

  implicit class RichTypeRelation(val relation: TypeRelation) extends AnyVal{
    def apply(s: String) = PartialTypeRestriction(relation, s)
    def <=(t2: Type) = Covariant(relation, t2)
  }


  case class PartialTypeRestriction(relation: TypeRelation, variable: String){
//    def =>:(t: Type) = TypeRestriction(relation, variable, t)
  }

  implicit def stringToTypeVariable(s: String): TypeVariable = TypeVariable(s)
  def lub(t1: Type, t2: Type): Type = LeastUpperBound(t1, t2)

  val top = TopType()
  val long = IntType()
  val int = IntType()
  val float = FloatType()
  val string = StringType()
  val boolean = BooleanType()

  val num = Alias("Num", long <= int <= float)
  val ord = num

  val numericOperation = num("x") =>: num("y") =>: "x" ~>: "y" ~>: lub("x", "y")
  val compareOperation = ord("x") =>: ord("y") =>: "x" ~>: "y" ~>: boolean

  val add = num("x") =>: num("y") =>: int
  val gt = ord("x") =>: "x" ~>: "x" ~>: boolean



  def reduceByApplication(fn: Type, arg: Type): Type = {
    fn match{
      case TypeRestriction(restriction, arg, t) => t
      case FunctionType(t1, t2) => t1 match{
        case TypeVariable(x) => substituteTypeVariable(x, t2, arg)
      }
      case _ => top
    }
  }

  def substituteTypeVariable(variable: String, t: Type, to: Type): Type = {
    t match{
      case TypeVariable(v) if v == variable => to
      case FunctionType(t1, t2) => FunctionType(
        substituteTypeVariable(variable, t1, to),
        substituteTypeVariable(variable, t2, to)
      )
      case LeastUpperBound(t1, t2) =>
        LeastUpperBound(
          substituteTypeVariable(variable, t1, to),
          substituteTypeVariable(variable, t2, to)
        )
//      case TypeRestriction(relation, variable, tpe) => TypeRestriction
      case t => t

    }
  }
}
