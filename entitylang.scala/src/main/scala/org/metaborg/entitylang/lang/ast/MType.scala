package org.metaborg.entitylang.lang.ast

object MType {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import org.metaborg.entitylang.lang.ast.MCommon._
  // Lexical definitions

  // Lexical extractors

  // Sort definitions
  sealed trait SType extends sdf.Constructor
  sealed trait STypeWithMultiplicity extends sdf.Constructor
  sealed trait SPrimitiveTypeWithMultiplicity extends sdf.Constructor
  sealed trait SPrimitiveType extends sdf.Constructor with SType
  sealed trait SMultiplicity extends sdf.Constructor
  // Constructor definitions
  object SType extends scalaterms.TermLikeCompanion[SType] {
    override val fromSTerm: scalaterms.FromSTerm[SType] = new scalaterms.FromSTerm[SType] {
      override def unapply(term: STerm): Option[SType] = term match {
        case SPrimitiveType.fromSTerm(_1) => scala.Some(_1)
        case _ => scala.None
      }
    }


  }
  object STypeWithMultiplicity extends scalaterms.TermLikeCompanion[STypeWithMultiplicity] {
    override val fromSTerm: scalaterms.FromSTerm[STypeWithMultiplicity] = new scalaterms.FromSTerm[STypeWithMultiplicity] {
      override def unapply(term: STerm): Option[STypeWithMultiplicity] = term match {
        case TypeWithMultiplicity2.fromSTerm(typewithmultiplicity1) => scala.Some(typewithmultiplicity1)
        case TypeWithDefaultMultiplicity1.fromSTerm(typewithmultiplicity1) => scala.Some(typewithmultiplicity1)
        case _ => scala.None
      }
    }

    case class TypeWithMultiplicity2(type1: SType, multiplicity2: SMultiplicity, origin: scalaterms.Origin) extends STypeWithMultiplicity {
      override def toSTerm = STerm.Cons("TypeWithMultiplicity", scala.Seq(type1.toSTerm, multiplicity2.toSTerm), scala.Some(origin))
    }
    object TypeWithMultiplicity2 extends scalaterms.TermLikeCompanion[TypeWithMultiplicity2] {
      override val fromSTerm: scalaterms.FromSTerm[TypeWithMultiplicity2] = new scalaterms.FromSTerm[TypeWithMultiplicity2] {
        override def unapply(term: STerm): Option[TypeWithMultiplicity2] = term match {
          case STerm.Cons("TypeWithMultiplicity", scala.Seq(SType.fromSTerm(type1), SMultiplicity.fromSTerm(multiplicity2)), scala.Some(origin)) =>
            scala.Some(TypeWithMultiplicity2(type1, multiplicity2, origin))
          case _ => None
        }
      }
    }
    case class TypeWithDefaultMultiplicity1(type1: SType, origin: scalaterms.Origin) extends STypeWithMultiplicity {
      override def toSTerm = STerm.Cons("TypeWithDefaultMultiplicity", scala.Seq(type1.toSTerm), scala.Some(origin))
    }
    object TypeWithDefaultMultiplicity1 extends scalaterms.TermLikeCompanion[TypeWithDefaultMultiplicity1] {
      override val fromSTerm: scalaterms.FromSTerm[TypeWithDefaultMultiplicity1] = new scalaterms.FromSTerm[TypeWithDefaultMultiplicity1] {
        override def unapply(term: STerm): Option[TypeWithDefaultMultiplicity1] = term match {
          case STerm.Cons("TypeWithDefaultMultiplicity", scala.Seq(SType.fromSTerm(type1)), scala.Some(origin)) =>
            scala.Some(TypeWithDefaultMultiplicity1(type1, origin))
          case _ => None
        }
      }
    }
  }
  object SPrimitiveTypeWithMultiplicity extends scalaterms.TermLikeCompanion[SPrimitiveTypeWithMultiplicity] {
    override val fromSTerm: scalaterms.FromSTerm[SPrimitiveTypeWithMultiplicity] = new scalaterms.FromSTerm[SPrimitiveTypeWithMultiplicity] {
      override def unapply(term: STerm): Option[SPrimitiveTypeWithMultiplicity] = term match {
        case PrimitiveTypeWithMultiplicity2.fromSTerm(primitivetypewithmultiplicity1) => scala.Some(primitivetypewithmultiplicity1)
        case PrimitiveTypeWithDefaultMultiplicity1.fromSTerm(primitivetypewithmultiplicity1) => scala.Some(primitivetypewithmultiplicity1)
        case _ => scala.None
      }
    }

    case class PrimitiveTypeWithMultiplicity2(primitivetype1: SPrimitiveType, multiplicity2: SMultiplicity, origin: scalaterms.Origin) extends SPrimitiveTypeWithMultiplicity {
      override def toSTerm = STerm.Cons("PrimitiveTypeWithMultiplicity", scala.Seq(primitivetype1.toSTerm, multiplicity2.toSTerm), scala.Some(origin))
    }
    object PrimitiveTypeWithMultiplicity2 extends scalaterms.TermLikeCompanion[PrimitiveTypeWithMultiplicity2] {
      override val fromSTerm: scalaterms.FromSTerm[PrimitiveTypeWithMultiplicity2] = new scalaterms.FromSTerm[PrimitiveTypeWithMultiplicity2] {
        override def unapply(term: STerm): Option[PrimitiveTypeWithMultiplicity2] = term match {
          case STerm.Cons("PrimitiveTypeWithMultiplicity", scala.Seq(SPrimitiveType.fromSTerm(primitivetype1), SMultiplicity.fromSTerm(multiplicity2)), scala.Some(origin)) =>
            scala.Some(PrimitiveTypeWithMultiplicity2(primitivetype1, multiplicity2, origin))
          case _ => None
        }
      }
    }
    case class PrimitiveTypeWithDefaultMultiplicity1(primitivetype1: SPrimitiveType, origin: scalaterms.Origin) extends SPrimitiveTypeWithMultiplicity {
      override def toSTerm = STerm.Cons("PrimitiveTypeWithDefaultMultiplicity", scala.Seq(primitivetype1.toSTerm), scala.Some(origin))
    }
    object PrimitiveTypeWithDefaultMultiplicity1 extends scalaterms.TermLikeCompanion[PrimitiveTypeWithDefaultMultiplicity1] {
      override val fromSTerm: scalaterms.FromSTerm[PrimitiveTypeWithDefaultMultiplicity1] = new scalaterms.FromSTerm[PrimitiveTypeWithDefaultMultiplicity1] {
        override def unapply(term: STerm): Option[PrimitiveTypeWithDefaultMultiplicity1] = term match {
          case STerm.Cons("PrimitiveTypeWithDefaultMultiplicity", scala.Seq(SPrimitiveType.fromSTerm(primitivetype1)), scala.Some(origin)) =>
            scala.Some(PrimitiveTypeWithDefaultMultiplicity1(primitivetype1, origin))
          case _ => None
        }
      }
    }
  }
  object SPrimitiveType extends scalaterms.TermLikeCompanion[SPrimitiveType] {
    override val fromSTerm: scalaterms.FromSTerm[SPrimitiveType] = new scalaterms.FromSTerm[SPrimitiveType] {
      override def unapply(term: STerm): Option[SPrimitiveType] = term match {
        case Boolean0.fromSTerm(primitivetype1) => scala.Some(primitivetype1)
        case Int0.fromSTerm(primitivetype1) => scala.Some(primitivetype1)
        case Float0.fromSTerm(primitivetype1) => scala.Some(primitivetype1)
        case String0.fromSTerm(primitivetype1) => scala.Some(primitivetype1)
        case _ => scala.None
      }
    }

    case class Boolean0(origin: scalaterms.Origin) extends SPrimitiveType {
      override def toSTerm = STerm.Cons("Boolean", scala.Seq(), scala.Some(origin))
    }
    object Boolean0 extends scalaterms.TermLikeCompanion[Boolean0] {
      override val fromSTerm: scalaterms.FromSTerm[Boolean0] = new scalaterms.FromSTerm[Boolean0] {
        override def unapply(term: STerm): Option[Boolean0] = term match {
          case STerm.Cons("Boolean", scala.Seq(), scala.Some(origin)) =>
            scala.Some(Boolean0(origin))
          case _ => None
        }
      }
    }
    case class Int0(origin: scalaterms.Origin) extends SPrimitiveType {
      override def toSTerm = STerm.Cons("Int", scala.Seq(), scala.Some(origin))
    }
    object Int0 extends scalaterms.TermLikeCompanion[Int0] {
      override val fromSTerm: scalaterms.FromSTerm[Int0] = new scalaterms.FromSTerm[Int0] {
        override def unapply(term: STerm): Option[Int0] = term match {
          case STerm.Cons("Int", scala.Seq(), scala.Some(origin)) =>
            scala.Some(Int0(origin))
          case _ => None
        }
      }
    }
    case class Float0(origin: scalaterms.Origin) extends SPrimitiveType {
      override def toSTerm = STerm.Cons("Float", scala.Seq(), scala.Some(origin))
    }
    object Float0 extends scalaterms.TermLikeCompanion[Float0] {
      override val fromSTerm: scalaterms.FromSTerm[Float0] = new scalaterms.FromSTerm[Float0] {
        override def unapply(term: STerm): Option[Float0] = term match {
          case STerm.Cons("Float", scala.Seq(), scala.Some(origin)) =>
            scala.Some(Float0(origin))
          case _ => None
        }
      }
    }
    case class String0(origin: scalaterms.Origin) extends SPrimitiveType {
      override def toSTerm = STerm.Cons("String", scala.Seq(), scala.Some(origin))
    }
    object String0 extends scalaterms.TermLikeCompanion[String0] {
      override val fromSTerm: scalaterms.FromSTerm[String0] = new scalaterms.FromSTerm[String0] {
        override def unapply(term: STerm): Option[String0] = term match {
          case STerm.Cons("String", scala.Seq(), scala.Some(origin)) =>
            scala.Some(String0(origin))
          case _ => None
        }
      }
    }
  }
  object SMultiplicity extends scalaterms.TermLikeCompanion[SMultiplicity] {
    override val fromSTerm: scalaterms.FromSTerm[SMultiplicity] = new scalaterms.FromSTerm[SMultiplicity] {
      override def unapply(term: STerm): Option[SMultiplicity] = term match {
        case One0.fromSTerm(multiplicity1) => scala.Some(multiplicity1)
        case ZeroOrOne0.fromSTerm(multiplicity1) => scala.Some(multiplicity1)
        case ZeroOrMore0.fromSTerm(multiplicity1) => scala.Some(multiplicity1)
        case OneOrMore0.fromSTerm(multiplicity1) => scala.Some(multiplicity1)
        case _ => scala.None
      }
    }

    case class One0(origin: scalaterms.Origin) extends SMultiplicity {
      override def toSTerm = STerm.Cons("One", scala.Seq(), scala.Some(origin))
    }
    object One0 extends scalaterms.TermLikeCompanion[One0] {
      override val fromSTerm: scalaterms.FromSTerm[One0] = new scalaterms.FromSTerm[One0] {
        override def unapply(term: STerm): Option[One0] = term match {
          case STerm.Cons("One", scala.Seq(), scala.Some(origin)) =>
            scala.Some(One0(origin))
          case _ => None
        }
      }
    }
    case class ZeroOrOne0(origin: scalaterms.Origin) extends SMultiplicity {
      override def toSTerm = STerm.Cons("ZeroOrOne", scala.Seq(), scala.Some(origin))
    }
    object ZeroOrOne0 extends scalaterms.TermLikeCompanion[ZeroOrOne0] {
      override val fromSTerm: scalaterms.FromSTerm[ZeroOrOne0] = new scalaterms.FromSTerm[ZeroOrOne0] {
        override def unapply(term: STerm): Option[ZeroOrOne0] = term match {
          case STerm.Cons("ZeroOrOne", scala.Seq(), scala.Some(origin)) =>
            scala.Some(ZeroOrOne0(origin))
          case _ => None
        }
      }
    }
    case class ZeroOrMore0(origin: scalaterms.Origin) extends SMultiplicity {
      override def toSTerm = STerm.Cons("ZeroOrMore", scala.Seq(), scala.Some(origin))
    }
    object ZeroOrMore0 extends scalaterms.TermLikeCompanion[ZeroOrMore0] {
      override val fromSTerm: scalaterms.FromSTerm[ZeroOrMore0] = new scalaterms.FromSTerm[ZeroOrMore0] {
        override def unapply(term: STerm): Option[ZeroOrMore0] = term match {
          case STerm.Cons("ZeroOrMore", scala.Seq(), scala.Some(origin)) =>
            scala.Some(ZeroOrMore0(origin))
          case _ => None
        }
      }
    }
    case class OneOrMore0(origin: scalaterms.Origin) extends SMultiplicity {
      override def toSTerm = STerm.Cons("OneOrMore", scala.Seq(), scala.Some(origin))
    }
    object OneOrMore0 extends scalaterms.TermLikeCompanion[OneOrMore0] {
      override val fromSTerm: scalaterms.FromSTerm[OneOrMore0] = new scalaterms.FromSTerm[OneOrMore0] {
        override def unapply(term: STerm): Option[OneOrMore0] = term match {
          case STerm.Cons("OneOrMore", scala.Seq(), scala.Some(origin)) =>
            scala.Some(OneOrMore0(origin))
          case _ => None
        }
      }
    }
  }
}