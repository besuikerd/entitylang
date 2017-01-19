package org.metaborg.entitylang.lang.ast

object Mentitylang {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import org.metaborg.entitylang.lang.ast.MCommon._
  import org.metaborg.entitylang.lang.ast.MModel._
  // Lexical definitions
  
  // Lexical extractors
  
  // Sort definitions
  sealed trait SStart extends sdf.Constructor
  // Constructor definitions
  object SStart extends scalaterms.TermLikeCompanion[SStart] {
    override val fromSTerm: scalaterms.FromSTerm[SStart] = new scalaterms.FromSTerm[SStart] {
      override def unapply(term: STerm): Option[SStart] = term match {
        case Start1.fromSTerm(start1) => scala.Some(start1)
        case _ => scala.None
      }
    }
  
    case class Start1(model1: STerm.List[SModel], origin: scalaterms.Origin) extends SStart {
      override def toSTerm = STerm.Cons("Start", scala.Seq(model1.toSTerm), scala.Some(origin))
    }
    object Start1 extends scalaterms.TermLikeCompanion[Start1] {
      override val fromSTerm: scalaterms.FromSTerm[Start1] = new scalaterms.FromSTerm[Start1] {
        override def unapply(term: STerm): Option[Start1] = term match {
          case STerm.Cons("Start", scala.Seq(SModel.fromSTerm.list(model1)), scala.Some(origin)) =>
            scala.Some(Start1(model1, origin))
          case _ => None
        }
      }
    }
  }
}