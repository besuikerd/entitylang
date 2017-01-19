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
  sealed trait SPrimitiveType extends sdf.Constructor with SType
  // Constructor definitions
  object SType extends scalaterms.TermLikeCompanion[SType] {
    override val fromSTerm: scalaterms.FromSTerm[SType] = new scalaterms.FromSTerm[SType] {
      override def unapply(term: STerm): Option[SType] = term match {
        case SPrimitiveType.fromSTerm(_1) => scala.Some(_1)
        case _ => scala.None
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
}