package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.MType.SMultiplicity
import org.metaborg.scalaterms.TermLike

import scalax.collection.{Graph, GraphEdge}

package object analysis {
  type AnalysisEdgeType[T] = GraphEdge.UnDiEdge[T]
  type AnalysisGraph = Graph[AnalysisGraphNode, AnalysisEdgeType]
  type AnalysisNode = AnalysisGraph#NodeT
  type AnalysisEdge = AnalysisGraph#EdgeT

  @inline implicit def richAnalysisGraph(analysisGraph: AnalysisGraph) = new RichAnalysisGraph(analysisGraph)
  @inline implicit def richAnalysisNode(analysisNode: AnalysisNode) = new RichAnalysisNode(analysisNode)

  sealed trait AnalysisGraphNode

  case class EntityNode(name: String) extends AnalysisGraphNode
  case class EntityNodeData(e: Entity2)

  sealed trait EntityFieldNode extends AnalysisGraphNode{
    val entity: String
    val name: String
  }

  sealed trait EntityFieldNodeData{
    type TermType <: TermLike
    val fieldType: Type
    val term: TermType
  }

  object EntityFieldNodeData{
    trait Aux[T <: TermLike] extends EntityFieldNodeData{
      type TermType = T
    }
  }

  case class DerivedValueNode(entity: String, name: String) extends EntityFieldNode
  case class DerivedValueNodeData(fieldType: Type, node: EntityFieldNode, term: DerivedAttribute3) extends EntityFieldNodeData.Aux[DerivedAttribute3]

  case class AttributeNode(entity: String, name: String) extends EntityFieldNode
  case class AttributeNodeData(fieldType : Type, node: EntityFieldNode, term: Attribute2) extends EntityFieldNodeData.Aux[Attribute2]

  case class RelationNode(entity: String, name: String) extends EntityFieldNode
  case class RelationNodeData(fieldType: EntityType, entityRef: EntityRef1, attributeRef: AttributeRef1, multiplicity: SMultiplicity, term: Relation6) extends EntityFieldNodeData.Aux[Relation6]
}
