package org.metaborg.entitylang.analysis.types

import org.metaborg.entitylang.lang.ast.MExpression.SExp
import org.metaborg.entitylang.lang.ast.MExpression.SExp.If3
import org.metaborg.entitylang.lang.ast.MExpression.SLiteral.True0
import org.metaborg.scalaterms.Origin

object TypeSystem {
  case class TypeError(origin: Origin, message: String)

  trait TypeSystem[TermType, TypeType]{
    def infer(ast: TermType): Either[TypeError, TypeType]
    def getOrigin(t: TermType): Origin
  }

  trait TypingRule {
    type TermType
    type TypeType
    type T <: TypeType

    type Rule[U <: TypeType] = TypingRule.Aux[TermType, TypeType, U]

    type TypeSystemT = TypeSystem[TermType, TypeType]
    type TypingResult = Either[TypeError, T]

    def widen[U >: T <: TypeType] = this.asInstanceOf[TypingRule.Aux[TermType, TypeType, U]]

    def typeError(term: TermType, msg: String)(implicit typeSystem: TypeSystemT): TypingResult = Left(TypeError(typeSystem.getOrigin(term), msg))
    def internalError(msg: String) = Left(TypeError(null, msg))

    def run(implicit typeSystem: TypeSystemT): TypingResult

    def map[U <: TypeType](f: T => U)(implicit typeSystem: TypeSystemT): Rule[U] = new MappedTypingRule[TermType, TypeType, T, U](this, f)
    def flatMap[U <: TypeType](f: T => Rule[U])(implicit typeSystem: TypeSystemT): Rule[U] = new FlatMappedTypingRule[TermType, TypeType, T, U](this, f)
  }

  object TypingRule{
    type Aux[TermType0, TypeType0, T0 <: TypeType0] = TypingRule{
      type TermType = TermType0
      type TypeType = TypeType0
      type T = T0
    }


  }

  class MappedTypingRule[TermType0, TypeType0, T <: TypeType0, U <: TypeType0](rule: TypingRule.Aux[TermType0, TypeType0, T], f: T => U)(implicit typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule {
    override type TermType = TermType0
    override type TypeType = TypeType0
    override type T = U
    override def run(implicit typeSystem: TypeSystemT): TypingResult = rule.run.right.map(t => f(t))
  }

  class FlatMappedTypingRule[TermType0, TypeType0, T <: TypeType0, U <: TypeType0](rule: TypingRule.Aux[TermType0, TypeType0, T], f: T => TypingRule.Aux[TermType0, TypeType0, U]) extends TypingRule{
    override type TermType = TermType0
    override type TypeType = TypeType0
    override type T = U
    override def run(implicit typeSystem: TypeSystemT): TypingResult = rule.run.right.flatMap(t => f(t).run)
  }

  class OfTypeTypingRule[TermType0, TypeType0, T0 <: TypeType0](e: TermType0, t1: T0)(implicit typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule{
    override type TermType = TermType0
    override type TypeType = TypeType0
    override type T = T0
    override def run(implicit typeSystem: TypeSystemT): TypingResult = {
      typeSystem.infer(e).right.flatMap(t2 => if(t1 == t2) Right(t1) else typeError(e, s"Expected type: $t1, got: $t2"))
    }
  }

  class SuccessTypingRule[TermType0, TypeType0, T0 <: TypeType0](t1: T0)(implicit  typeSystem: TypeSystem[TermType0, TypeType0]) extends TypingRule{
    override type TermType = TermType0
    override type TypeType = TypeType0
    override type T = T0

    override def run(implicit typeSystem: TypeSystemT): TypingResult = Right(t1)
  }


  type TopLevelTypingRule[TermType0, TypeType0] = PartialFunction[TermType0, TypingRule{type TermType = TermType0; type TypeType = TypeType0}]
  object TopLevelTypingRule{
    def apply[TermType0, TypeType0](pf: TopLevelTypingRule[TermType0, TypeType0])(
      implicit typeSystem: TypeSystem[TermType0, TypeType0]
    ): TopLevelTypingRule[TermType0, TypeType0] = pf
  }


  implicit val typeSystem = new TypeSystem[SExp, Type] {
    lazy val rules = Seq[TopLevelTypingRule[SExp, Type]](if3, true0).map(_.andThen(_.widen[Type]))
    override def infer(ast: SExp): Either[TypeError, Type] =
      rules
        .view
        .map(pf => pf.andThen(x => x.run(this)).applyOrElse(ast, (_: SExp) => Left(TypeError(null, "Rule failed"))))
        .collectFirst{case Right(t) => t}.toRight(TypeError(null, "Could not find valid rule to apply"))
    override def getOrigin(t: SExp): Origin = t.origin
  }

  val if3: TopLevelTypingRule[SExp, Type] = TopLevelTypingRule[SExp, Type]{
    case If3(e1, e2, e3, _) =>
      e1 hasType boolean
  }

  val true0: TopLevelTypingRule[SExp, Type] = TopLevelTypingRule[SExp, Type]{
    case True0(_) => new SuccessTypingRule(boolean)
  }

  implicit class RichTypeRules[TermType, TypeType](val term: TermType)(implicit typeSystem: TypeSystem[TermType, TypeType]){
    type Rule[T <: TypeType] = TypingRule.Aux[TermType, TypeType, T]


    def hasType[T <: TypeType](t: T): Rule[T] = new OfTypeTypingRule(term, t)
  }

//  class TopLevelTypingRule[TermType0, TypeType0](
//    term: TermType0,
//    pf: PartialFunction[TermType0, TypingRule{type TermType = TermType0; type TypeType = TypeType0}]
//  )(
//    implicit typeSystem: TypeSystem[TermType0, TypeType0]
//  ) extends TypingRule{
//    override type TermType = TermType0
//    override type TypeType = TypeType0
//    override type T = TypeType
//
//    override def run(implicit typeSystem: TypeSystemT): TypingResult = {
//      pf.andThen(_.widen.run).apply(term)
//    }
//  }

  type Rule[T <: SExp, U <: Type] = T => TypingRule{
    type TermType = SExp
    type TypeType = Type
  }

//  val if3: Rule[If3, Type] = {
//    case If3(e1, e2, e3, _) =>
//      val x = for{
//        t1 <- e1 hasType boolean
//      } yield t1
//      x
//  }

  println(if3)

//
//  class MatchingTypingRule[TermType, TypeType](terms: TermType*) extends TypingRule[TermType, TypeType, TypeType] {
//    override def run(implicit typeSystem: TypeSystemT) : TypingResult = {
//      if(terms.nonEmpty) {
//        internalError("Empty set of matching rules")
//      } else if(terms.size == 1){
//        typeSystem.infer(terms.head)
//      } else{
//        terms.tail.foldLeft(typeSystem.infer(terms.head)) {
//          case (Left(s), _) => Left(s)
//          case (Right(t1), e) => typeSystem.infer(e) match{
//            case Right(t2) => if (t1 == t2) Right(t1) else typeError(e, s"Expected type: $t1, got: $t2")
//            case otherwise => otherwise
//          }
//        }
//      }
//    }
//  }
//
//  def matching[TermType, TypeType](terms: TermType*)(implicit typeSystem: TypeSystem[TermType, TypeType]): TypingRule[TermType, TypeType, TypeType] = new MatchingTypingRule(terms:_*)

//  def rule[TermType, TypeType, T <: TypeType](pf: PartialFunction[TermType,  TypingRule[TermType, TypeType, T]])(
//    implicit typeSystem: TypeSystem[TermType, TypeType]
//  ): TermType => Either[TypeError, T] =
//    t => pf.andThen(_.run).applyOrElse(t, term => Left(TypeError(typeSystem.getOrigin(term), "Mismatched rule")))


//  val not = rule[Not1](n => for(t1 <- ofType[BooleanType](n.exp1)) yield t1)



//  def specRule[T <: SExp](pf: PartialFunction[SExp, TypingRule[SExp, Type, T]]) = rule[SExp, Type, T](pf)




//  type Rule[T <: Type] = SExp => TypingRule[SExp, Type, T]
//
//  val if3: Rule[Type] = {
//    case If3(exp1, exp2, exp3, origin) =>
//
//      val r = for{
//        t1 <- ofType(exp1, boolean)
//        t2 <- matching(
//          exp1,
//          exp2
//        )(typeSystem)
//      } yield t2
//
//      r
//  }



//  def infer(e: SExp) = e match {
//    case _: SLiteral =>
//    case MemberAccess2(exp1, id2, origin) =>
//    case Not1(exp1, origin) =>
//    case BinExp(op, e1, e2) =>
//    case If3(exp1, exp2, exp3, origin) =>
//    case Apply2(exp1, exp2, origin) =>
//    case Ref1(id1, origin) =>
//    case This0(origin) =>
//  }
}
