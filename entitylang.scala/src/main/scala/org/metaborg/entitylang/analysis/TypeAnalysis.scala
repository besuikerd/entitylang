package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.analysis.DataflowAnalysis._
import org.metaborg.entitylang.desugar
import org.metaborg.entitylang.desugar._
import org.metaborg.entitylang.lang.ast.MExpression.SExp._
import org.metaborg.entitylang.lang.ast.MExpression.SLiteral._
import org.metaborg.entitylang.lang.ast.MExpression.{SExp, SLiteral}
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.slf4j.LoggerFactory

object TypeAnalysis {

  sealed trait MultiplicityType

  object MultiplicityType{
    //merge

    def merge(m1: BaseMultiplicityType, m2: BaseMultiplicityType): BaseMultiplicityType = (m1, m2) match{
      case (_: MaybeEmpty, _: MaybeEmpty) => ZeroOrMore
      case _ => OneOrMore
    }

    def choiceLeft(m1: BaseMultiplicityType, m2: BaseMultiplicityType): BaseMultiplicityType = m2
  }

  trait NonEmpty
  trait MaybeEmpty

  case object NonZero extends MultiplicityType
  case object MaybeZero extends MultiplicityType

  sealed trait BaseMultiplicityType extends MultiplicityType

  case object One extends BaseMultiplicityType with NonEmpty
  case object ZeroOrOne extends BaseMultiplicityType with MaybeEmpty
  case object ZeroOrMore extends BaseMultiplicityType with MaybeEmpty
  case object OneOrMore extends BaseMultiplicityType with NonEmpty

  case class MultiplicityVariable(s: String) extends MultiplicityType
  case class Merge(m1: MultiplicityType, m2: MultiplicityType) extends MultiplicityType



  sealed trait Typeclass
  case class Ord(t: Type) extends Typeclass
  case class Num(t: Type)  extends Typeclass

  sealed trait BaseType
  case class StringType() extends BaseType
  case class BooleanType() extends BaseType
  case class IntType() extends BaseType
  case class FloatType() extends BaseType
  case class TypeVariable(s: String) extends BaseType

  sealed trait Type
  case class FunctionType(arg: Type, res: Type) extends Type
  case object Top extends Type
  case class MType(baseType: BaseType, multiplicityType: MultiplicityType) extends Type

  object Type{
    val int = IntType()
    val float = FloatType()
    val string = StringType()
    val boolean = BooleanType()

    val num: BaseType = int
    val ord: BaseType = int
    val show: BaseType = int

    def tvar(name: String) = TypeVariable(name)
  }

  import Type._


  val binaryOperatorTypes : Map[BinaryOperator, Type] = Map(
    Add -> num.m("m1") ~>: num.m("m1") ~>: num.m("m1"),
    Sub -> num ~>: num ~>: num,
    Mul -> num ~>: num ~>: num,
    Div -> num ~>: num ~>: num,
    Mod -> num ~>: num ~>: num,
    LessThan -> ord ~>: ord ~>: ord,
    LessThanEqual -> ord ~>: ord ~>: ord,
    GreaterThan -> ord ~>: ord ~>: ord,
    GreaterThanEqual -> ord ~>: ord ~>: ord,
    Equal -> ord ~>: ord ~>: ord,
    Inequal -> ord ~>: ord ~>: ord
//    And -> ,
//    Or -> ,
//    desugar.Merge -> ,
//    ChoiceLeft ->
  )

  implicit class BaseTypeImplicits(t: BaseType) extends AnyRef{
    def _1 = MType(t, One)
    def `0?` = MType(t, MaybeZero)
    def * = MType(t, ZeroOrMore)
    def + = MType(t, OneOrMore)
    def ? = MType(t, ZeroOrOne)
    def ~>:(t2: Type): Type = t._1 ~>: t2
    def ~>:(t2: BaseType): Type = t._1 ~>: t2._1
    def m(name: String) = MType(t, MultiplicityVariable(name))

  }

  implicit class TypeImplicits(t: Type) extends AnyRef{
    def ~>:(t2: Type): Type = FunctionType(t, t2)
    def ~>:(t2: BaseType): Type = t ~>: t2._1
  }

  implicit class MultiplicityImplicits(m: MultiplicityType) extends AnyRef{
    def ++ (m2: MultiplicityType) = Merge(m, m2)
  }

  val choiceLeft =
    FunctionType(
      MType(TypeVariable("t1"), MaybeZero),
      FunctionType(
        MType(TypeVariable("t1"), MultiplicityVariable("m1")),
        MType(TypeVariable("t1"), MultiplicityVariable("m1"))
      )
    )

  val merge =
    FunctionType(
      MType(TypeVariable("t1"), MultiplicityVariable("m1")),
      FunctionType(
        MType(TypeVariable("t1"), MultiplicityVariable("m2")),
        MType(TypeVariable("t1"), Merge(MultiplicityVariable("m1"), MultiplicityVariable("m2")))
      )
    )


  case class EntityType(
    attributes: Map[String, Type]
  )


  trait FreshVariableGenerator[A]{
    def generate(): A
  }

  implicit val freshVariableGenerator = new FreshVariableGenerator[Int] {
    private var index = 0
    override def generate(): Int = {
      val res = index
      index += 1
      res
    }
  }

  def binaryOperatorType(op: BinaryOperator) = op match {
    case Add => int ~>: int ~>: int
  }


  def getType(env: Map[String, Type])(e: SExp): Type = e match {
    case BinExp(op, e1, e2) => Top
    case Int1(_, _) => int._1
    case String1(_, _) => int._1
    case True0(_) => int._1
    case False0(_) => boolean._1
    case Float1(_, _) => float._1

    case Apply2(e1, e2, _) =>
      val t1 = getType(env)(e1)
      val t2 = e2.value.map(getType(env))
      t2.foldLeft(t1) {
        case (FunctionType(arg, res), cur) if arg == cur => res
        case _ => Top
    }
    case Ref1(id1, _) => env.getOrElse(id1.string, Top)
  }


  def collectConstraints(s: Start1): Unit = {

//    val res = s.model1.value.flatMap{
//      case Entity2(id1, member2, origin) => member2.value.flatMap{
//        case DerivedAttribute3(id, tpe, e, _) =>
//        case Attribute2(id, tpe, _)
//      }
//      case Relation6(entityref1, attributeref2, multiplicity3, multiplicity4, entityref5, attributeref6, origin) => Seq.empty
//    }
  }

  /*
   */

  case class BinaryOperatorType(left: Type, right: Type, result: Type)
  case class UnaryOperatorType(in: Type, out: Type)

//  def binaryOperatorType(op: BinaryOperator): BinaryOperatorType = op match {
//    case Add => BinaryOperatorType(IntType(), IntType(), IntType())
//    case Sub => BinaryOperatorType(IntType(), IntType(), IntType())
//    case Mul => BinaryOperatorType(IntType(), IntType(), IntType())
//    case Div => BinaryOperatorType(IntType(), IntType(), IntType())
//    case Mod => BinaryOperatorType(IntType(), IntType(), IntType())
//    case LessThan => BinaryOperatorType(IntType(), IntType(), BooleanType())
//    case LessThanEqual => BinaryOperatorType(IntType(), IntType(), BooleanType())
//    case GreaterThan => BinaryOperatorType(IntType(), IntType(), BooleanType())
//    case GreaterThanEqual => BinaryOperatorType(IntType(), IntType(), BooleanType())
//    case Equal => BinaryOperatorType(IntType(), IntType(), BooleanType())
//    case Inequal => BinaryOperatorType(IntType(), IntType(), BooleanType())
//    case And => BinaryOperatorType(BooleanType(), BooleanType(), BooleanType())
//    case Or => BinaryOperatorType(BooleanType(), BooleanType(), BooleanType())
//    case Merge => BinaryOperatorType(MultiplicityType())
//    case ChoiceLeft =>
//  }


  def collectConstraints(e: SExp): Unit = {
    e match {
      case _: SLiteral =>
      case MemberAccess2(exp1, id2, origin) =>
      case BinExp(op, e1, e2) =>
      case Not1(exp1, origin) =>
      case Multiplication2(exp1, exp2, origin) =>
      case Division2(exp1, exp2, origin) =>
      case Modulo2(exp1, exp2, origin) =>
      case Addition2(exp1, exp2, origin) =>
      case Subtraction2(exp1, exp2, origin) =>
      case LessThan2(exp1, exp2, origin) =>
      case LessThanEqual2(exp1, exp2, origin) =>
      case GreaterThan2(exp1, exp2, origin) =>
      case GreaterThanEqual2(exp1, exp2, origin) =>
      case Equal2(exp1, exp2, origin) =>
      case Inequal2(exp1, exp2, origin) =>
      case And2(exp1, exp2, origin) =>
      case Or2(exp1, exp2, origin) =>
      case If3(exp1, exp2, exp3, origin) =>
      case Merge2(exp1, exp2, origin) =>
      case ChoiceLeft2(exp1, exp2, origin) =>
      case Apply2(exp1, exp2, origin) =>
      case Ref1(id1, origin) =>
      case This0(origin) =>
    }
  }


  val logger = LoggerFactory.getLogger(getClass)

  def topologicalSort(d: DataflowGraph): Seq[Attribute] = {
    var l = Seq.empty
    var s = d.nodes.filter(n => !d.edges.exists(e => e.to == n.index))
    logger.info(s.toString())
    s.foreach(n => logger.debug(n.index.toString))

    null
//    while(!s.isEmpty){
//      val n = s.head
//      s = s.tail
//
//    }


  }
}
