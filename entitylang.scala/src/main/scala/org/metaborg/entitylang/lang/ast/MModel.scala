package org.metaborg.entitylang.lang.ast

object MModel {
  // Generic imports
  import org.metaborg.scalaterms
  import org.metaborg.scalaterms.{ sdf, STerm }
  // Generated imports
  import org.metaborg.entitylang.lang.ast.MCommon._
  import org.metaborg.entitylang.lang.ast.MType._
  import org.metaborg.entitylang.lang.ast.MExpression._
  // Lexical definitions

  // Lexical extractors

  // Sort definitions
  sealed trait SModel extends sdf.Constructor
  sealed trait SMember extends sdf.Constructor
  sealed trait SOptionalType extends sdf.Constructor
  sealed trait SAttribute extends sdf.Constructor with SMember
  sealed trait SEntityRef extends sdf.Constructor
  sealed trait SAttributeRef extends sdf.Constructor
  // Constructor definitions
  object SModel extends scalaterms.TermLikeCompanion[SModel] {
    override val fromSTerm: scalaterms.FromSTerm[SModel] = new scalaterms.FromSTerm[SModel] {
      override def unapply(term: STerm): Option[SModel] = term match {
        case Entity2.fromSTerm(model1) => scala.Some(model1)
        case Relation6.fromSTerm(model1) => scala.Some(model1)
        case _ => scala.None
      }
    }

    case class Entity2(id1: SID, member2: STerm.List[SMember], origin: scalaterms.Origin) extends SModel {
      override def toSTerm = STerm.Cons("Entity", scala.Seq(id1.toSTerm, member2.toSTerm), scala.Some(origin))
    }
    object Entity2 extends scalaterms.TermLikeCompanion[Entity2] {
      override val fromSTerm: scalaterms.FromSTerm[Entity2] = new scalaterms.FromSTerm[Entity2] {
        override def unapply(term: STerm): Option[Entity2] = term match {
          case STerm.Cons("Entity", scala.Seq(SID.fromSTerm(id1), SMember.fromSTerm.list(member2)), scala.Some(origin)) =>
            scala.Some(Entity2(id1, member2, origin))
          case _ => None
        }
      }
    }
    case class Relation6(entityref1: SEntityRef, attributeref2: SAttributeRef, multiplicity3: SMultiplicity, multiplicity4: SMultiplicity, entityref5: SEntityRef, attributeref6: SAttributeRef, origin: scalaterms.Origin) extends SModel {
      override def toSTerm = STerm.Cons("Relation", scala.Seq(entityref1.toSTerm, attributeref2.toSTerm, multiplicity3.toSTerm, multiplicity4.toSTerm, entityref5.toSTerm, attributeref6.toSTerm), scala.Some(origin))
    }
    object Relation6 extends scalaterms.TermLikeCompanion[Relation6] {
      override val fromSTerm: scalaterms.FromSTerm[Relation6] = new scalaterms.FromSTerm[Relation6] {
        override def unapply(term: STerm): Option[Relation6] = term match {
          case STerm.Cons("Relation", scala.Seq(SEntityRef.fromSTerm(entityref1), SAttributeRef.fromSTerm(attributeref2), SMultiplicity.fromSTerm(multiplicity3), SMultiplicity.fromSTerm(multiplicity4), SEntityRef.fromSTerm(entityref5), SAttributeRef.fromSTerm(attributeref6)), scala.Some(origin)) =>
            scala.Some(Relation6(entityref1, attributeref2, multiplicity3, multiplicity4, entityref5, attributeref6, origin))
          case _ => None
        }
      }
    }
  }
  object SMember extends scalaterms.TermLikeCompanion[SMember] {
    override val fromSTerm: scalaterms.FromSTerm[SMember] = new scalaterms.FromSTerm[SMember] {
      override def unapply(term: STerm): Option[SMember] = term match {
        case SAttribute.fromSTerm(_1) => scala.Some(_1)
        case _ => scala.None
      }
    }


  }
  object SOptionalType extends scalaterms.TermLikeCompanion[SOptionalType] {
    override val fromSTerm: scalaterms.FromSTerm[SOptionalType] = new scalaterms.FromSTerm[SOptionalType] {
      override def unapply(term: STerm): Option[SOptionalType] = term match {
        case ExplicitType1.fromSTerm(optionaltype1) => scala.Some(optionaltype1)
        case DerivedType0.fromSTerm(optionaltype1) => scala.Some(optionaltype1)
        case _ => scala.None
      }
    }

    case class ExplicitType1(primitivetypewithmultiplicity1: SPrimitiveTypeWithMultiplicity, origin: scalaterms.Origin) extends SOptionalType {
      override def toSTerm = STerm.Cons("ExplicitType", scala.Seq(primitivetypewithmultiplicity1.toSTerm), scala.Some(origin))
    }
    object ExplicitType1 extends scalaterms.TermLikeCompanion[ExplicitType1] {
      override val fromSTerm: scalaterms.FromSTerm[ExplicitType1] = new scalaterms.FromSTerm[ExplicitType1] {
        override def unapply(term: STerm): Option[ExplicitType1] = term match {
          case STerm.Cons("ExplicitType", scala.Seq(SPrimitiveTypeWithMultiplicity.fromSTerm(primitivetypewithmultiplicity1)), scala.Some(origin)) =>
            scala.Some(ExplicitType1(primitivetypewithmultiplicity1, origin))
          case _ => None
        }
      }
    }
    case class DerivedType0(origin: scalaterms.Origin) extends SOptionalType {
      override def toSTerm = STerm.Cons("DerivedType", scala.Seq(), scala.Some(origin))
    }
    object DerivedType0 extends scalaterms.TermLikeCompanion[DerivedType0] {
      override val fromSTerm: scalaterms.FromSTerm[DerivedType0] = new scalaterms.FromSTerm[DerivedType0] {
        override def unapply(term: STerm): Option[DerivedType0] = term match {
          case STerm.Cons("DerivedType", scala.Seq(), scala.Some(origin)) =>
            scala.Some(DerivedType0(origin))
          case _ => None
        }
      }
    }
  }
  object SAttribute extends scalaterms.TermLikeCompanion[SAttribute] {
    override val fromSTerm: scalaterms.FromSTerm[SAttribute] = new scalaterms.FromSTerm[SAttribute] {
      override def unapply(term: STerm): Option[SAttribute] = term match {
        case Attribute2.fromSTerm(attribute1) => scala.Some(attribute1)
        case DerivedAttribute3.fromSTerm(attribute1) => scala.Some(attribute1)
        case _ => scala.None
      }
    }

    case class Attribute2(id1: SID, primitivetypewithmultiplicity2: SPrimitiveTypeWithMultiplicity, origin: scalaterms.Origin) extends SAttribute {
      override def toSTerm = STerm.Cons("Attribute", scala.Seq(id1.toSTerm, primitivetypewithmultiplicity2.toSTerm), scala.Some(origin))
    }
    object Attribute2 extends scalaterms.TermLikeCompanion[Attribute2] {
      override val fromSTerm: scalaterms.FromSTerm[Attribute2] = new scalaterms.FromSTerm[Attribute2] {
        override def unapply(term: STerm): Option[Attribute2] = term match {
          case STerm.Cons("Attribute", scala.Seq(SID.fromSTerm(id1), SPrimitiveTypeWithMultiplicity.fromSTerm(primitivetypewithmultiplicity2)), scala.Some(origin)) =>
            scala.Some(Attribute2(id1, primitivetypewithmultiplicity2, origin))
          case _ => None
        }
      }
    }
    case class DerivedAttribute3(id1: SID, optionaltype2: SOptionalType, exp3: SExp, origin: scalaterms.Origin) extends SAttribute {
      override def toSTerm = STerm.Cons("DerivedAttribute", scala.Seq(id1.toSTerm, optionaltype2.toSTerm, exp3.toSTerm), scala.Some(origin))
    }
    object DerivedAttribute3 extends scalaterms.TermLikeCompanion[DerivedAttribute3] {
      override val fromSTerm: scalaterms.FromSTerm[DerivedAttribute3] = new scalaterms.FromSTerm[DerivedAttribute3] {
        override def unapply(term: STerm): Option[DerivedAttribute3] = term match {
          case STerm.Cons("DerivedAttribute", scala.Seq(SID.fromSTerm(id1), SOptionalType.fromSTerm(optionaltype2), SExp.fromSTerm(exp3)), scala.Some(origin)) =>
            scala.Some(DerivedAttribute3(id1, optionaltype2, exp3, origin))
          case _ => None
        }
      }
    }
  }
  object SEntityRef extends scalaterms.TermLikeCompanion[SEntityRef] {
    override val fromSTerm: scalaterms.FromSTerm[SEntityRef] = new scalaterms.FromSTerm[SEntityRef] {
      override def unapply(term: STerm): Option[SEntityRef] = term match {
        case EntityRef1.fromSTerm(entityref1) => scala.Some(entityref1)
        case _ => scala.None
      }
    }

    case class EntityRef1(id1: SID, origin: scalaterms.Origin) extends SEntityRef {
      override def toSTerm = STerm.Cons("EntityRef", scala.Seq(id1.toSTerm), scala.Some(origin))
    }
    object EntityRef1 extends scalaterms.TermLikeCompanion[EntityRef1] {
      override val fromSTerm: scalaterms.FromSTerm[EntityRef1] = new scalaterms.FromSTerm[EntityRef1] {
        override def unapply(term: STerm): Option[EntityRef1] = term match {
          case STerm.Cons("EntityRef", scala.Seq(SID.fromSTerm(id1)), scala.Some(origin)) =>
            scala.Some(EntityRef1(id1, origin))
          case _ => None
        }
      }
    }
  }
  object SAttributeRef extends scalaterms.TermLikeCompanion[SAttributeRef] {
    override val fromSTerm: scalaterms.FromSTerm[SAttributeRef] = new scalaterms.FromSTerm[SAttributeRef] {
      override def unapply(term: STerm): Option[SAttributeRef] = term match {
        case AttributeRef1.fromSTerm(attributeref1) => scala.Some(attributeref1)
        case _ => scala.None
      }
    }

    case class AttributeRef1(id1: SID, origin: scalaterms.Origin) extends SAttributeRef {
      override def toSTerm = STerm.Cons("AttributeRef", scala.Seq(id1.toSTerm), scala.Some(origin))
    }
    object AttributeRef1 extends scalaterms.TermLikeCompanion[AttributeRef1] {
      override val fromSTerm: scalaterms.FromSTerm[AttributeRef1] = new scalaterms.FromSTerm[AttributeRef1] {
        override def unapply(term: STerm): Option[AttributeRef1] = term match {
          case STerm.Cons("AttributeRef", scala.Seq(SID.fromSTerm(id1)), scala.Some(origin)) =>
            scala.Some(AttributeRef1(id1, origin))
          case _ => None
        }
      }
    }
  }
}