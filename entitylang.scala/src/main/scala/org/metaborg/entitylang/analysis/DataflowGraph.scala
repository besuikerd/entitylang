package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.analysis.DataflowAnalysis.Attribute
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.MType.SMultiplicity
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.scalaterms.{STerm, TermLike}


object DataflowGraph {
  import scalax.collection.Graph
  import scalax.collection.GraphEdge
  import scalax.collection.GraphPredef._

  type DataflowGraph = Graph[EntityField, GraphEdge.DiEdge]

  sealed trait EntityField{
    type TermType <: TermLike
    val term: TermType
  }

  object EntityField{
    trait Aux[T <: TermLike] extends EntityField{
      type TermType = T
    }
  }

  sealed trait EntityRelation extends EntityField.Aux[Relation6]{
    val entityRef: EntityRef1
    val attributeRef: AttributeRef1
    val multiplicity: SMultiplicity
  }

  object EntityRelation{
    def unapply(field: EntityField): Option[(EntityRef1, AttributeRef1, SMultiplicity, Relation6)] = field match {
      case e : EntityRelation => Some(e.entityRef, e.attributeRef, e.multiplicity, e.term)
      case _ => None
    }
  }

  case class EntityAttribute(term: Attribute2) extends EntityField.Aux[Attribute2]
  case class EntityDerivedValue(term: DerivedAttribute3) extends EntityField.Aux[DerivedAttribute3]
  case class EntityRelationLeft(entityRef: EntityRef1, attributeRef: AttributeRef1, multiplicity: SMultiplicity, term: Relation6) extends EntityRelation
  case class EntityRelationRight(entityRef: EntityRef1, attributeRef: AttributeRef1, multiplicity: SMultiplicity, term: Relation6) extends EntityRelation

  def buildDataflowGraph(ast: Start1): DataflowGraph = {

    //add all nodes to the graph
    val graph = ast.model1.value.foldLeft[DataflowGraph](Graph.empty){
      case (graph, model) => model match {
        case Entity2(id1, member2, origin) =>
          val members: Seq[EntityField] = member2.value.map{
            case attribute: Attribute2 => EntityAttribute(attribute)
            case derivedValue: DerivedAttribute3 => EntityDerivedValue(derivedValue)
          }
          graph.union(Graph(members:_*))
        case relation @ Relation6(entityRefLeft, attributeRefLeft, multiplicityLeft, multiplicityRight, entityRefRight, attributeRefRight, _) => Graph(
          EntityRelationLeft(entityRefLeft.asInstanceOf[EntityRef1], attributeRefLeft.asInstanceOf[AttributeRef1], multiplicityLeft, relation),
          EntityRelationRight(entityRefRight.asInstanceOf[EntityRef1], attributeRefRight.asInstanceOf[AttributeRef1], multiplicityRight, relation)
        )
      }
    }

    graph
  }

}
