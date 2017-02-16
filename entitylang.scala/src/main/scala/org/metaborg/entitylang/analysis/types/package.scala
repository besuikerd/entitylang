package org.metaborg.entitylang.analysis


import org.metaborg.entitylang.util.MapExtensions.SeqValuesMapExtensions

package object types {

  def ppType(t: TypeWithEnvironment): String = {
    t.env.constraints.mkString("\n") + "\n" + ppType(t.tpe)
  }
  def ppType(t: Type): String = t match {
    case StringType() => "string"
    case BooleanType() => "boolean"
    case IntType() => "int"
    case LongType() => "long"
    case FloatType() => "float"
    case EntityType(name) => name
    case TopType() => "⊤"
    case FunctionType(t1, t2) => s"${ppType(t1)} -> ${ppType(t2)}"
    case LeastUpperBound(t1, t2) => s"(${ppType(t1)} | ${ppType(t2)})"
    case TypeVariable(name) => "$" + name
    case TypeRestriction(relation, variable, tpe) => s"${ppTypeConstraint(relation)} $variable => ${ppType(tpe)}"
    case t: TypeConstraint => ppTypeConstraint(t)
  }

  def ppTypeConstraint(t: TypeConstraint): String = t match {
    case Covariant(t1, t2) => s"${ppTypeConstraint(t1)} <= ${ppType(t2)}"
    case Alternative(t1, t2) => s"${ppType(t1)} | ${ppType(t2)}"
    case Invariant(t) => ppType(t)
    case TypeClass(name) => name
  }


  sealed trait Type{
    def ~>(t2: Type) = FunctionType(this, t2)
    def ~>:(t2: Type) = t2 ~> this
  }

  sealed trait NumericType extends Type
  implicit val numericOrdering: Ordering[NumericType] = new Ordering[NumericType]{
    override def compare(x: NumericType, y: NumericType): Int = orderNum(x) - orderNum(y)

    def orderNum(t: NumericType) = t match {
      case IntType() => 0
      case LongType() => 1
      case FloatType() => 2
    }
  }

  case class StringType() extends Type
  case class BooleanType() extends Type
  case class LongType() extends NumericType
  case class IntType() extends NumericType
  case class FloatType() extends NumericType
  case class EntityType(name: String) extends Type
  case class TopType() extends Type
  case class FunctionType(t1: Type, t2: Type) extends Type

  case class LeastUpperBound(t1: TypeVariable, t2: TypeVariable) extends Type
  case class TypeVariable(name: String) extends Type {
    def |(t2: TypeVariable) = LeastUpperBound(this, t2)
  }
  case class TypeRestriction(relation: TypeConstraint, variable: String, tpe: Type) extends Type


  sealed trait TypeConstraint extends Type
//  case class Alias(name: String, relation: TypeConstraint) extends TypeConstraint
  case class Covariant(t1: Invariant, t2: TypeConstraint) extends TypeConstraint
  case class Alternative(t1: TypeConstraint, t2: TypeConstraint) extends TypeConstraint
  case class Invariant(t: Type) extends TypeConstraint
  case class TypeClass(name: String) extends TypeConstraint

  implicit class RichType(val t: Type) extends AnyVal{
    def <=(t2: TypeConstraint) = Covariant(Invariant(t), t2)
    def <=(t2: Type) = Covariant(Invariant(t), Invariant(t2))
    def <+(t2: TypeConstraint) = Alternative(Invariant(t), t2)
    def <+(t2: Type) = Alternative(Invariant(t), Invariant(t2))

    def ~>(t2: Type) = FunctionType(t, t2)

    def =>:(partialTypeRestriction: PartialTypeRestriction) = TypeRestriction(partialTypeRestriction.relation, partialTypeRestriction.variable, t)
  }

  implicit class RichTypeConstraint(val constraint: TypeConstraint) extends AnyVal{
    def apply(s: String) = PartialTypeRestriction(constraint, s)
    def <+(t2: TypeConstraint) = Alternative(constraint, t2)
    def <+(t2: Type) = Alternative(constraint, Invariant(t2))
//    def <=(t2: Type) = Covariant(relation, t2)
//    def <+(t2: TypeConstraint) = Alternative(relation, t2)
  }

  implicit class RichInvariant(val invariant: Invariant) extends AnyVal{
    def <=(t2: TypeConstraint) = Covariant(invariant, t2)
    def <=(t2: Type) = Covariant(invariant, Invariant(t2))
  }



  case class PartialTypeRestriction(relation: TypeConstraint, variable: String){
    def =>:(t: Type) = TypeRestriction(relation, variable, t)
  }

  implicit def stringToTypeVariable(s: String): TypeVariable = TypeVariable(s)
  def lub(t1: TypeVariable, t2: TypeVariable): Type = LeastUpperBound(t1, t2)


  val top = TopType()
  val long = LongType()
  val int = IntType()
  val float = FloatType()
  val string = StringType()
  val boolean = BooleanType()



  val num = TypeClass("Num") //Alias("Num", int <= long <= float)
  val show = TypeClass("Show")
//  val show = Alias("Show", num <+ string <+ boolean)
  val ord = num

  val numericOperation = num("x") =>: num("y") =>: "x" ~>: "y" ~>: lub("x", "y")
  val compareOperation = ord("x") =>: ord("y") =>: "x" ~>: "y" ~>: boolean
  val concatOperation = show("x") =>: show("y") =>: "x" ~>: "y" ~>: string
  val equality = "x" ~>: "x" ~>: boolean
  val add = numericOperation <+ concatOperation

  val gt = ord("x") =>: "x" ~>: "x" ~>: boolean

  case class TypingEnvironment(
    constraints: Map[String, Seq[TypeConstraint]],
    typeClasses: Map[String, TypeConstraint]
  ) {

    def top(variable: String): TypingEnvironment = copy(constraints = constraints.addBinding(variable, Invariant(types.top)))
    def bind(variable: String, tpe: Type): TypingEnvironment = copy(constraints = constraints.addBinding(variable, Invariant(tpe)))
  }

  object TypingEnvironment{
    def apply(): TypingEnvironment = TypingEnvironment(Map.empty, defaultTypeClasses)

    val defaultTypeClasses: Map[String, TypeConstraint] = Map(
      num,
      show
    )
    def num = "Num" -> (int <= long <= float)
    def show = "Show" -> (TypeClass("Num") <+ string <+ boolean)
  }

  case class TypeWithEnvironment(tpe: Type, env: TypingEnvironment){
    def reduce(arg: Type): TypeWithEnvironment = types.reduce(tpe, arg)(env)
  }

  object TypeWithEnvironment{
    def bind(t: Type)(implicit env: TypingEnvironment): TypeWithEnvironment = TypeWithEnvironment(t, env)
    def reduce(t: Type, arg: Type)(implicit env: TypingEnvironment): TypeWithEnvironment = TypeWithEnvironment(t, env).reduce(arg)
  }

  def reduce(fn: Type, arg: Type)(implicit env: TypingEnvironment): TypeWithEnvironment = {
    println("reducing " + ppType(fn) + " " + fn)

    def collectConstraints(fn: Type, arg: Type)(implicit env: TypingEnvironment): TypeWithEnvironment = fn match{
      case TypeRestriction(restriction, variable, t) =>
        collectConstraints(t, arg)(env.copy(constraints = env.constraints.addBinding(variable, restriction)))
      case FunctionType(t1, t2) => t1 match{
        case TypeVariable(x) =>
          val violations = verifyRestrictions(x, arg)
          if(violations.isEmpty) {
            TypeWithEnvironment(t2, env.bind(x, arg))
          } else {
            println(violations)
            TypeWithEnvironment(t2, env.top(x))
          }
      }
      case Alternative(t1, t2) => reduce(t1, arg) match {
        case TypeWithEnvironment(TopType(), _) => reduce(t2, arg)
        case t => {println(t); t}
    }
      case Invariant(t) => reduce(t, arg)
      case _ => TypeWithEnvironment.bind(top)
    }
    val constrainedType = collectConstraints(fn, arg)
    resolveConstrainedType(constrainedType)
  }

  def verifyRestrictions(variable: String, tpe: Type)(implicit env: TypingEnvironment): Seq[TypeConstraint] = {
    def verifyRestriction(tpe: Type)(restriction: TypeConstraint) : Seq[TypeConstraint] = restriction match {
      case Covariant(t1, t2) =>
        val violations = verifyRestriction(tpe)(t1)
        if(violations.isEmpty) violations else verifyRestriction(tpe)(t2)
      case Alternative(t1, t2) =>
        val violations = verifyRestriction(tpe)(t1)
        if(violations.isEmpty) violations else verifyRestriction(tpe)(t2)
      case i @ Invariant(t) =>
        if(t == tpe) Seq.empty else Seq(i)
      case c @ TypeClass(name) =>
        env.typeClasses.get(name).map(verifyRestriction(tpe)).getOrElse(Seq(c))
    }

    env.constraints.get(variable).toSeq.flatten.flatMap(verifyRestriction(tpe))
  }

  def resolveConstrainedType(t: TypeWithEnvironment): TypeWithEnvironment = resolveConstrainedType(t.tpe)(t.env)
  def resolveConstrainedType(t: Type)(implicit env: TypingEnvironment): TypeWithEnvironment = t match {
    case LeastUpperBound(v1, v2) =>
      val lub = for {
        constraints1 <- env.constraints.get(v1.name)
        constraints2 <- env.constraints.get(v2.name)
      } yield findLeastUpperBound(constraints1, constraints2)
      TypeWithEnvironment.bind(lub.flatMap(_.right.toOption).getOrElse(top))
    case TypeVariable(name) =>
      val tpe = env.constraints.get(name).flatMap{ constraints =>
      constraints.collectFirst{case Invariant(tpe) => tpe}
    }.getOrElse(top)
      TypeWithEnvironment.bind(tpe)
    case otherwise => TypeWithEnvironment.bind(otherwise)
  }

  def findLeastUpperBound(c1: Seq[TypeConstraint], c2: Seq[TypeConstraint])(implicit env: TypingEnvironment): Either[TypeConstraint, Type] = {

    val constraints = c1.toSet ++ c2.toSet
    println(constraints)



    case class InCo(t: Type, co: Covariant)
    case class And(c1: TypeConstraint, c2: TypeConstraint)

    val x: (TypeConstraint, TypeConstraint) = null



    x match{
      case (i: Invariant, a: Alternative) => And(i, a)
      case (Invariant(t), co @ Covariant(Invariant(t2), r)) =>
        if(t == t2) {
          InCo(t, co)
        } else {

          //find t in rhs // otherwise fail
        }
      case (Invariant(t), Invariant(t2)) => if(t == t2) Invariant(t) else { //fail }
    } }


    Right(top)

//    def prune(t: Type, branch: TypeConstraint): Option[Type] = {
//      branch match{
//        case Invariant(t2) if t == t2 => Some(t)
//        case Alternative(left, right) => prune(t, left).map((prune(t, right))
//        case Covariant(left, right) =>
//        case _ => None
//      }
//    }
//
//    def lub(c1: TypeConstraint, c2: TypeConstraint): Type = {
//      (c1, c2) match{
//        case (Invariant(t), Covariant(left, right)) =>
//        case (Invariant(t), Alternative(left, right)) =>
//        case (i1: Invariant, i2: Invariant) => Alternative(i1, i2)
//        case (t: TypeClass, c2) => lub(env.typeClasses.get(t.name).get, c2)
//        case (c1, t: TypeClass) => lub(c1, env.typeClasses.get(t.name).get)
//      }
//
//
//      val res = constraint match {
//        case co @ Covariant(t1, t2) => tpe match{
//
//        }
//        case Alternative(t1, t2) => lub(tpe, t1).left.flatMap(_ => lub(tpe, t2))
//        case i @ Invariant(t) => if(t == tpe) i else Right(Alternative(Invariant(tpe), i))
//        case cls @ TypeClass(name) => env.typeClasses.get(name).toRight(cls).right.flatMap(c => lub(tpe, c))
//      }
//      res
//    }
//
//    constraints.foldLeft[Either[TypeConstraint, Type]](Right(top)){
//      case (acc, constraint) => acc.right.flatMap(t => lub(t, constraint))
//    }
  }





}
