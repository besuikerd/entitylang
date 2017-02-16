package org.metaborg.entitylang

import com.besuikerd.hmap.{HMap, Relation}
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.MModel.SOptionalType
import org.metaborg.entitylang.lang.ast.MModel.SOptionalType.{DerivedType0, ExplicitType1}
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveType.{Boolean0, Float0, Int0, String0}
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveTypeWithMultiplicity.{PrimitiveTypeWithDefaultMultiplicity1, PrimitiveTypeWithMultiplicity2}
import org.metaborg.entitylang.lang.ast.MType.{SMultiplicity, SPrimitiveType, SPrimitiveTypeWithMultiplicity}
import org.metaborg.scalaterms.TermLike

import scalax.collection.{Graph, GraphEdge}

package object analysis {
  sealed trait NodeDataMap[K, V]
  object NodeDataMap extends Relation{

    implicit case object MapEntityNode extends NodeDataMap[EntityNode, EntityNodeData]
    implicit case object MapDerivedValueNode extends NodeDataMap[DerivedValueNode, DerivedValueNodeData]
    implicit case object MapAttribute extends NodeDataMap[AttributeNode, AttributeNodeData]
    implicit case object MapRelation extends NodeDataMap[RelationNode, RelationNodeData]

    val emptyMap = HMap.empty[NodeDataMap](
      MapEntityNode,
      MapDerivedValueNode,
      MapAttribute,
      MapRelation
    )

    override type Relation[K, V] = emptyMap.Relation[K, V]
    override type MapType = emptyMap.MapType
    override def empty = emptyMap
  }

  sealed trait BaseType

  sealed trait NumericType

  case class StringType() extends BaseType
  case class BooleanType() extends BaseType
  case class IntType() extends BaseType with NumericType
  case class FloatType() extends BaseType with NumericType
  case class EntityType(name: String) extends BaseType
  case class BottomType() extends BaseType

  object BaseType{
    val string = StringType()
    val boolean = BooleanType()
    val int = IntType()
    val float = FloatType()
    val bottom = BottomType()

    def apply(t: SOptionalType): BaseType = t match {
      case ExplicitType1(primitivetypewithmultiplicity1, origin) => apply(primitivetypewithmultiplicity1)
      case DerivedType0(origin) => bottom
    }

    def apply(t: SPrimitiveTypeWithMultiplicity): BaseType = t match {
      case PrimitiveTypeWithMultiplicity2(primitivetype1, multiplicity2, origin) => apply(primitivetype1)
      case PrimitiveTypeWithDefaultMultiplicity1(primitivetype1, origin) => apply(primitivetype1)
    }

    def apply(t: SPrimitiveType): BaseType = t match {
      case Boolean0(origin) => boolean
      case Int0(origin) => int
      case Float0(origin) => float
      case String0(origin) => string
    }
  }

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
    val fieldType: BaseType
    val term: TermType
  }

  object EntityFieldNodeData{
    trait Aux[T <: TermLike] extends EntityFieldNodeData{
      type TermType = T
    }
  }

  case class DerivedValueNode(entity: String, name: String) extends EntityFieldNode
  case class DerivedValueNodeData(fieldType: BaseType, node: EntityFieldNode, term: DerivedAttribute3) extends EntityFieldNodeData.Aux[DerivedAttribute3]

  case class AttributeNode(entity: String, name: String) extends EntityFieldNode
  case class AttributeNodeData(fieldType : BaseType, node: EntityFieldNode, term: Attribute2) extends EntityFieldNodeData.Aux[Attribute2]

  case class RelationNode(entity: String, name: String) extends EntityFieldNode
  case class RelationNodeData(fieldType: EntityType, entityRef: EntityRef1, attributeRef: AttributeRef1, multiplicity: SMultiplicity, term: Relation6) extends EntityFieldNodeData.Aux[Relation6]
}
