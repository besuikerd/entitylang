package org.metaborg.entitylang.lang.ast

object MExpression {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import org.metaborg.entitylang.lang.ast.MCommon._
  // Lexical definitions

  // Lexical extractors

  // Sort definitions
  sealed trait SLiteral extends sdf.Constructor with SExp
  sealed trait SExp extends sdf.Constructor
  // Constructor definitions
  object SLiteral extends scalaterms.TermLikeCompanion[SLiteral] {
    override val fromSTerm: scalaterms.FromSTerm[SLiteral] = new scalaterms.FromSTerm[SLiteral] {
      override def unapply(term: STerm): Option[SLiteral] = term match {
        case Int1.fromSTerm(literal1) => scala.Some(literal1)
        case Float1.fromSTerm(literal1) => scala.Some(literal1)
        case String1.fromSTerm(literal1) => scala.Some(literal1)
        case True0.fromSTerm(literal1) => scala.Some(literal1)
        case False0.fromSTerm(literal1) => scala.Some(literal1)
        case _ => scala.None
      }
    }

    case class Int1(int1: SINT, origin: scalaterms.Origin) extends SLiteral {
      override def toSTerm = STerm.Cons("Int", scala.Seq(int1.toSTerm), scala.Some(origin))
    }
    object Int1 extends scalaterms.TermLikeCompanion[Int1] {
      override val fromSTerm: scalaterms.FromSTerm[Int1] = new scalaterms.FromSTerm[Int1] {
        override def unapply(term: STerm): Option[Int1] = term match {
          case STerm.Cons("Int", scala.Seq(SINT.fromSTerm(int1)), scala.Some(origin)) =>
            scala.Some(Int1(int1, origin))
          case _ => None
        }
      }
    }
    case class Float1(float1: SFLOAT, origin: scalaterms.Origin) extends SLiteral {
      override def toSTerm = STerm.Cons("Float", scala.Seq(float1.toSTerm), scala.Some(origin))
    }
    object Float1 extends scalaterms.TermLikeCompanion[Float1] {
      override val fromSTerm: scalaterms.FromSTerm[Float1] = new scalaterms.FromSTerm[Float1] {
        override def unapply(term: STerm): Option[Float1] = term match {
          case STerm.Cons("Float", scala.Seq(SFLOAT.fromSTerm(float1)), scala.Some(origin)) =>
            scala.Some(Float1(float1, origin))
          case _ => None
        }
      }
    }
    case class String1(string1: SSTRING, origin: scalaterms.Origin) extends SLiteral {
      override def toSTerm = STerm.Cons("String", scala.Seq(string1.toSTerm), scala.Some(origin))
    }
    object String1 extends scalaterms.TermLikeCompanion[String1] {
      override val fromSTerm: scalaterms.FromSTerm[String1] = new scalaterms.FromSTerm[String1] {
        override def unapply(term: STerm): Option[String1] = term match {
          case STerm.Cons("String", scala.Seq(SSTRING.fromSTerm(string1)), scala.Some(origin)) =>
            scala.Some(String1(string1, origin))
          case _ => None
        }
      }
    }
    case class True0(origin: scalaterms.Origin) extends SLiteral {
      override def toSTerm = STerm.Cons("True", scala.Seq(), scala.Some(origin))
    }
    object True0 extends scalaterms.TermLikeCompanion[True0] {
      override val fromSTerm: scalaterms.FromSTerm[True0] = new scalaterms.FromSTerm[True0] {
        override def unapply(term: STerm): Option[True0] = term match {
          case STerm.Cons("True", scala.Seq(), scala.Some(origin)) =>
            scala.Some(True0(origin))
          case _ => None
        }
      }
    }
    case class False0(origin: scalaterms.Origin) extends SLiteral {
      override def toSTerm = STerm.Cons("False", scala.Seq(), scala.Some(origin))
    }
    object False0 extends scalaterms.TermLikeCompanion[False0] {
      override val fromSTerm: scalaterms.FromSTerm[False0] = new scalaterms.FromSTerm[False0] {
        override def unapply(term: STerm): Option[False0] = term match {
          case STerm.Cons("False", scala.Seq(), scala.Some(origin)) =>
            scala.Some(False0(origin))
          case _ => None
        }
      }
    }
  }
  object SExp extends scalaterms.TermLikeCompanion[SExp] {
    override val fromSTerm: scalaterms.FromSTerm[SExp] = new scalaterms.FromSTerm[SExp] {
      override def unapply(term: STerm): Option[SExp] = term match {
        case MemberAccess2.fromSTerm(exp1) => scala.Some(exp1)
        case Not1.fromSTerm(exp1) => scala.Some(exp1)
        case Multiplication2.fromSTerm(exp1) => scala.Some(exp1)
        case Division2.fromSTerm(exp1) => scala.Some(exp1)
        case Modulo2.fromSTerm(exp1) => scala.Some(exp1)
        case Addition2.fromSTerm(exp1) => scala.Some(exp1)
        case Subtraction2.fromSTerm(exp1) => scala.Some(exp1)
        case LessThan2.fromSTerm(exp1) => scala.Some(exp1)
        case LessThanEqual2.fromSTerm(exp1) => scala.Some(exp1)
        case GreaterThan2.fromSTerm(exp1) => scala.Some(exp1)
        case GreaterThanEqual2.fromSTerm(exp1) => scala.Some(exp1)
        case Equal2.fromSTerm(exp1) => scala.Some(exp1)
        case Inequal2.fromSTerm(exp1) => scala.Some(exp1)
        case And2.fromSTerm(exp1) => scala.Some(exp1)
        case Or2.fromSTerm(exp1) => scala.Some(exp1)
        case If3.fromSTerm(exp1) => scala.Some(exp1)
        case Merge2.fromSTerm(exp1) => scala.Some(exp1)
        case ChoiceLeft2.fromSTerm(exp1) => scala.Some(exp1)
        case Apply2.fromSTerm(exp1) => scala.Some(exp1)
        case Ref1.fromSTerm(exp1) => scala.Some(exp1)
        case Null0.fromSTerm(exp1) => scala.Some(exp1)
        case SLiteral.fromSTerm(_1) => scala.Some(_1)
        case _ => scala.None
      }
    }

    case class MemberAccess2(exp1: SExp, id2: SID, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("MemberAccess", scala.Seq(exp1.toSTerm, id2.toSTerm), scala.Some(origin))
    }
    object MemberAccess2 extends scalaterms.TermLikeCompanion[MemberAccess2] {
      override val fromSTerm: scalaterms.FromSTerm[MemberAccess2] = new scalaterms.FromSTerm[MemberAccess2] {
        override def unapply(term: STerm): Option[MemberAccess2] = term match {
          case STerm.Cons("MemberAccess", scala.Seq(SExp.fromSTerm(exp1), SID.fromSTerm(id2)), scala.Some(origin)) =>
            scala.Some(MemberAccess2(exp1, id2, origin))
          case _ => None
        }
      }
    }
    case class Not1(exp1: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Not", scala.Seq(exp1.toSTerm), scala.Some(origin))
    }
    object Not1 extends scalaterms.TermLikeCompanion[Not1] {
      override val fromSTerm: scalaterms.FromSTerm[Not1] = new scalaterms.FromSTerm[Not1] {
        override def unapply(term: STerm): Option[Not1] = term match {
          case STerm.Cons("Not", scala.Seq(SExp.fromSTerm(exp1)), scala.Some(origin)) =>
            scala.Some(Not1(exp1, origin))
          case _ => None
        }
      }
    }
    case class Multiplication2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Multiplication", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Multiplication2 extends scalaterms.TermLikeCompanion[Multiplication2] {
      override val fromSTerm: scalaterms.FromSTerm[Multiplication2] = new scalaterms.FromSTerm[Multiplication2] {
        override def unapply(term: STerm): Option[Multiplication2] = term match {
          case STerm.Cons("Multiplication", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Multiplication2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Division2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Division", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Division2 extends scalaterms.TermLikeCompanion[Division2] {
      override val fromSTerm: scalaterms.FromSTerm[Division2] = new scalaterms.FromSTerm[Division2] {
        override def unapply(term: STerm): Option[Division2] = term match {
          case STerm.Cons("Division", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Division2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Modulo2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Modulo", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Modulo2 extends scalaterms.TermLikeCompanion[Modulo2] {
      override val fromSTerm: scalaterms.FromSTerm[Modulo2] = new scalaterms.FromSTerm[Modulo2] {
        override def unapply(term: STerm): Option[Modulo2] = term match {
          case STerm.Cons("Modulo", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Modulo2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Addition2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Addition", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Addition2 extends scalaterms.TermLikeCompanion[Addition2] {
      override val fromSTerm: scalaterms.FromSTerm[Addition2] = new scalaterms.FromSTerm[Addition2] {
        override def unapply(term: STerm): Option[Addition2] = term match {
          case STerm.Cons("Addition", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Addition2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Subtraction2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Subtraction", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Subtraction2 extends scalaterms.TermLikeCompanion[Subtraction2] {
      override val fromSTerm: scalaterms.FromSTerm[Subtraction2] = new scalaterms.FromSTerm[Subtraction2] {
        override def unapply(term: STerm): Option[Subtraction2] = term match {
          case STerm.Cons("Subtraction", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Subtraction2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class LessThan2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("LessThan", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object LessThan2 extends scalaterms.TermLikeCompanion[LessThan2] {
      override val fromSTerm: scalaterms.FromSTerm[LessThan2] = new scalaterms.FromSTerm[LessThan2] {
        override def unapply(term: STerm): Option[LessThan2] = term match {
          case STerm.Cons("LessThan", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(LessThan2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class LessThanEqual2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("LessThanEqual", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object LessThanEqual2 extends scalaterms.TermLikeCompanion[LessThanEqual2] {
      override val fromSTerm: scalaterms.FromSTerm[LessThanEqual2] = new scalaterms.FromSTerm[LessThanEqual2] {
        override def unapply(term: STerm): Option[LessThanEqual2] = term match {
          case STerm.Cons("LessThanEqual", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(LessThanEqual2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class GreaterThan2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("GreaterThan", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object GreaterThan2 extends scalaterms.TermLikeCompanion[GreaterThan2] {
      override val fromSTerm: scalaterms.FromSTerm[GreaterThan2] = new scalaterms.FromSTerm[GreaterThan2] {
        override def unapply(term: STerm): Option[GreaterThan2] = term match {
          case STerm.Cons("GreaterThan", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(GreaterThan2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class GreaterThanEqual2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("GreaterThanEqual", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object GreaterThanEqual2 extends scalaterms.TermLikeCompanion[GreaterThanEqual2] {
      override val fromSTerm: scalaterms.FromSTerm[GreaterThanEqual2] = new scalaterms.FromSTerm[GreaterThanEqual2] {
        override def unapply(term: STerm): Option[GreaterThanEqual2] = term match {
          case STerm.Cons("GreaterThanEqual", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(GreaterThanEqual2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Equal2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Equal", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Equal2 extends scalaterms.TermLikeCompanion[Equal2] {
      override val fromSTerm: scalaterms.FromSTerm[Equal2] = new scalaterms.FromSTerm[Equal2] {
        override def unapply(term: STerm): Option[Equal2] = term match {
          case STerm.Cons("Equal", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Equal2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Inequal2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Inequal", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Inequal2 extends scalaterms.TermLikeCompanion[Inequal2] {
      override val fromSTerm: scalaterms.FromSTerm[Inequal2] = new scalaterms.FromSTerm[Inequal2] {
        override def unapply(term: STerm): Option[Inequal2] = term match {
          case STerm.Cons("Inequal", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Inequal2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class And2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("And", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object And2 extends scalaterms.TermLikeCompanion[And2] {
      override val fromSTerm: scalaterms.FromSTerm[And2] = new scalaterms.FromSTerm[And2] {
        override def unapply(term: STerm): Option[And2] = term match {
          case STerm.Cons("And", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(And2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Or2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Or", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Or2 extends scalaterms.TermLikeCompanion[Or2] {
      override val fromSTerm: scalaterms.FromSTerm[Or2] = new scalaterms.FromSTerm[Or2] {
        override def unapply(term: STerm): Option[Or2] = term match {
          case STerm.Cons("Or", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Or2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class If3(exp1: SExp, exp2: SExp, exp3: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("If", scala.Seq(exp1.toSTerm, exp2.toSTerm, exp3.toSTerm), scala.Some(origin))
    }
    object If3 extends scalaterms.TermLikeCompanion[If3] {
      override val fromSTerm: scalaterms.FromSTerm[If3] = new scalaterms.FromSTerm[If3] {
        override def unapply(term: STerm): Option[If3] = term match {
          case STerm.Cons("If", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2), SExp.fromSTerm(exp3)), scala.Some(origin)) =>
            scala.Some(If3(exp1, exp2, exp3, origin))
          case _ => None
        }
      }
    }
    case class Merge2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Merge", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Merge2 extends scalaterms.TermLikeCompanion[Merge2] {
      override val fromSTerm: scalaterms.FromSTerm[Merge2] = new scalaterms.FromSTerm[Merge2] {
        override def unapply(term: STerm): Option[Merge2] = term match {
          case STerm.Cons("Merge", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(Merge2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class ChoiceLeft2(exp1: SExp, exp2: SExp, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("ChoiceLeft", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object ChoiceLeft2 extends scalaterms.TermLikeCompanion[ChoiceLeft2] {
      override val fromSTerm: scalaterms.FromSTerm[ChoiceLeft2] = new scalaterms.FromSTerm[ChoiceLeft2] {
        override def unapply(term: STerm): Option[ChoiceLeft2] = term match {
          case STerm.Cons("ChoiceLeft", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm(exp2)), scala.Some(origin)) =>
            scala.Some(ChoiceLeft2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Apply2(exp1: SExp, exp2: STerm.List[SExp], origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Apply", scala.Seq(exp1.toSTerm, exp2.toSTerm), scala.Some(origin))
    }
    object Apply2 extends scalaterms.TermLikeCompanion[Apply2] {
      override val fromSTerm: scalaterms.FromSTerm[Apply2] = new scalaterms.FromSTerm[Apply2] {
        override def unapply(term: STerm): Option[Apply2] = term match {
          case STerm.Cons("Apply", scala.Seq(SExp.fromSTerm(exp1), SExp.fromSTerm.list(exp2)), scala.Some(origin)) =>
            scala.Some(Apply2(exp1, exp2, origin))
          case _ => None
        }
      }
    }
    case class Ref1(id1: SID, origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Ref", scala.Seq(id1.toSTerm), scala.Some(origin))
    }
    object Ref1 extends scalaterms.TermLikeCompanion[Ref1] {
      override val fromSTerm: scalaterms.FromSTerm[Ref1] = new scalaterms.FromSTerm[Ref1] {
        override def unapply(term: STerm): Option[Ref1] = term match {
          case STerm.Cons("Ref", scala.Seq(SID.fromSTerm(id1)), scala.Some(origin)) =>
            scala.Some(Ref1(id1, origin))
          case _ => None
        }
      }
    }
    case class Null0(origin: scalaterms.Origin) extends SExp {
      override def toSTerm = STerm.Cons("Null", scala.Seq(), scala.Some(origin))
    }
    object Null0 extends scalaterms.TermLikeCompanion[Null0] {
      override val fromSTerm: scalaterms.FromSTerm[Null0] = new scalaterms.FromSTerm[Null0] {
        override def unapply(term: STerm): Option[Null0] = term match {
          case STerm.Cons("Null", scala.Seq(), scala.Some(origin)) =>
            scala.Some(Null0(origin))
          case _ => None
        }
      }
    }
  }
}