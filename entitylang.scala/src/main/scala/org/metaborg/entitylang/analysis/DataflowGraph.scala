package org.metaborg.entitylang.analysis

import org.metaborg.entitylang.analysis.DataflowAnalysis.Attribute
import org.metaborg.entitylang.desugar.{BinExp, UnExp}
import org.metaborg.entitylang.lang.ast.MExpression.SExp._
import org.metaborg.entitylang.lang.ast.MExpression.{SExp, SLiteral}
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.MModel.SOptionalType
import org.metaborg.entitylang.lang.ast.MModel.SOptionalType.{DerivedType0, ExplicitType1}
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveType.{Boolean0, Float0, Int0, String0}
import org.metaborg.entitylang.lang.ast.MType.SPrimitiveTypeWithMultiplicity.{PrimitiveTypeWithDefaultMultiplicity1, PrimitiveTypeWithMultiplicity2}
import org.metaborg.entitylang.lang.ast.MType.{SMultiplicity, SPrimitiveType, SPrimitiveTypeWithMultiplicity, SType}
import org.metaborg.entitylang.lang.ast.Mentitylang.SStart.Start1
import org.metaborg.scalaterms.{STerm, TermLike}


object DataflowGraph {
  import scalax.collection.Graph
  import scalax.collection.GraphEdge
  import scalax.collection.GraphPredef._

  type DataflowEdgeType[T] = GraphEdge.UnDiEdge[T]
  type DataflowEdge = DataflowEdgeType[DataflowGraphNode]
  type DataflowGraph = Graph[DataflowGraphNode, DataflowEdgeType]

  sealed trait BaseType
  case class StringType() extends BaseType
  case class BooleanType() extends BaseType
  case class IntType() extends BaseType
  case class FloatType() extends BaseType
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

  sealed trait DataflowGraphNode

  implicit class RichDataflowGraphNode(val graph: DataflowGraph) extends AnyRef{
    def findEntity(name: String): Option[graph.NodeT] = graph.find(EntityNode(name))

    def findEntityData(name: String): Option[graph.NodeT] =
      findEntity(name)
        .flatMap(_.findSuccessor(_.value.isInstanceOf[EntityNodeData]))

    def findEntityField(entity: String, field: String): Option[graph.NodeT] =
      findEntity(entity)
        .flatMap(_.findSuccessor(_.value.isInstanceOf[EntityFieldNode]))

    def findEntityFieldData(entity: String, field: String): Option[graph.NodeT] =
      findEntityField(entity, field)
        .flatMap(_.findSuccessor(_.value.isInstanceOf[EntityFieldNodeData]))

  }


  case class EntityNode(name: String) extends DataflowGraphNode
  case class EntityNodeData(entity: Entity2) extends DataflowGraphNode

  case class EntityFieldNode(name: String) extends DataflowGraphNode
  sealed trait EntityFieldNodeData extends DataflowGraphNode {
    type TermType <: TermLike
    val fieldType: BaseType
    val term: TermType
  }

  object EntityFieldNodeData{
    trait Aux[T <: TermLike] extends EntityFieldNodeData{
      type TermType = T
    }
  }

  sealed trait EntityRelation extends EntityFieldNodeData.Aux[Relation6]{
    val entityRef: EntityRef1
    val attributeRef: AttributeRef1
    val multiplicity: SMultiplicity
  }

  object EntityRelation{
    def unapply(field: EntityFieldNodeData): Option[(EntityRef1, AttributeRef1, SMultiplicity, Relation6)] = field match {
      case e : EntityRelation => Some(e.entityRef, e.attributeRef, e.multiplicity, e.term)
      case _ => None
    }
  }

  case class EntityAttribute(fieldType : BaseType, term: Attribute2) extends EntityFieldNodeData.Aux[Attribute2]
  case class EntityDerivedValue(fieldType: BaseType, term: DerivedAttribute3) extends EntityFieldNodeData.Aux[DerivedAttribute3]
  case class EntityRelationLeft(fieldType: EntityType, entityRef: EntityRef1, attributeRef: AttributeRef1, multiplicity: SMultiplicity, term: Relation6) extends EntityRelation
  case class EntityRelationRight(fieldType: EntityType, entityRef: EntityRef1, attributeRef: AttributeRef1, multiplicity: SMultiplicity, term: Relation6) extends EntityRelation

  def buildDataflowGraph(ast: Start1): DataflowGraph = {

    //add all nodes to the graph
    val graph = ast.model1.value.foldLeft[DataflowGraph](Graph.empty){
      case (graph, model) => model match {
        case e @ Entity2(id1, member2, origin) => {
          val entityNode = EntityNode(id1.string)
          val entityNodeData = EntityNodeData(e)

          val members: Seq[DataflowEdge] = member2.value.map{
            case attribute @ Attribute2(name, tpe, origin) =>
              val entityFieldNode = EntityFieldNode(name.string)
              val entityFieldData = EntityAttribute(BaseType(tpe), attribute)
              entityFieldNode ~ entityFieldData
            case derivedValue @ DerivedAttribute3(name, tpe, e, origin) =>
              val entityFieldNode = EntityFieldNode(name.string)
              val entityFieldNodeData = EntityDerivedValue(BaseType(tpe), derivedValue)
              entityFieldNode ~ entityFieldNodeData
          }
          graph + entityNode + entityNodeData ++ members
        }
        case relation @ Relation6(entityRefLeft: EntityRef1, attributeRefLeft: AttributeRef1, multiplicityLeft, multiplicityRight, entityRefRight: EntityRef1, attributeRefRight: AttributeRef1, _) =>
          val fieldNodeLeft = EntityFieldNode(attributeRefLeft.id1.string)
          val fieldNodeRight = EntityFieldNode(attributeRefRight.id1.string)
          val edges = Graph(
            EntityNode(entityRefLeft.id1.string) ~ fieldNodeLeft,
            fieldNodeLeft ~ EntityRelationLeft(EntityType(entityRefRight.id1.string), entityRefLeft, attributeRefLeft, multiplicityLeft, relation),
            EntityNode(entityRefRight.id1.string) ~ fieldNodeRight,
            fieldNodeRight ~ EntityRelationRight(EntityType(entityRefLeft.id1.string), entityRefRight, attributeRefRight, multiplicityRight, relation)
          )
          graph ++ edges
      }
    }

    graph.nodes.foldLeft(graph){
      case (graph, node) => node.value match {
        case derivedValue @ EntityDerivedValue(a, b) => {

          val edge: DataflowEdge = derivedValue.copy(fieldType = BaseType.int) ~ node.findSuccessor(_.value.isInstanceOf[EntityFieldNode]).get
          graph - node + edge
        }
        case _ => graph
      }
    }.toOuterNodes.foreach(println)

    graph
  }


//  def dependencyEdges(n: DataflowGraph#NodeT, e: SExp): (DataflowGraph#NodeT, Seq[DataflowGraph#EdgeT]) = e match {
//
//    case MemberAccess2(e1, name, _) =>
//      val (m, edges) = dependencyEdges(n, e1)
//    case UnExp(_, e1) =>
//    case BinExp(_, e1, e2) =>
//      val (_, edges1) = dependencyEdges(n, e1)
//      val (_, edges2) = dependencyEdges(n, e2)
//      (n, edges1 ++ edges2)
//    case Apply2(exp1, exp2, origin) =>
//    case Ref1(id1, origin) =>
//    case _ => (n, Seq.empty)
//  }
}
