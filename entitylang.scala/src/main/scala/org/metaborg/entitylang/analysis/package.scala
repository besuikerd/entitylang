package org.metaborg.entitylang

import org.metaborg.entitylang.analysis.types._
import org.metaborg.entitylang.lang.ast.MModel.SAttribute.{Attribute2, DerivedAttribute3}
import org.metaborg.entitylang.lang.ast.MModel.SAttributeRef.AttributeRef1
import org.metaborg.entitylang.lang.ast.MModel.SEntityRef.EntityRef1
import org.metaborg.entitylang.lang.ast.MModel.SModel.{Entity2, Relation6}
import org.metaborg.entitylang.lang.ast.MType.SMultiplicity
import org.metaborg.scalaterms.sdf.Lexical
import org.metaborg.scalaterms.{HasOrigin, TermLike}

import scalax.collection.GraphEdge.{DiEdge, DiEdgeLike}
import scalax.collection.Graph
import scalax.collection.GraphPredef.{DiEdgeLikeIn, EdgeLikeIn}
import scalax.collection.edge.LkDiEdge

package object analysis {
  type AnalysisEdgeType[T] = LkDiEdge[T]
  type AnalysisGraph = Graph[EntityFieldNode, AnalysisEdgeType]
  type AnalysisNode = AnalysisGraph#NodeT
  type AnalysisEdge = AnalysisEdgeType[EntityFieldNode]

  @inline implicit def richAnalysisGraph(analysisGraph: AnalysisGraph) = new RichAnalysisGraph(analysisGraph)
  @inline implicit def richAnalysisNode(analysisNode: AnalysisNode) = new RichAnalysisNode(analysisNode)

  case class EntityFieldNode(
    entity: String,
    name: String
  )

  sealed trait EntityFieldNodeData{
    type TermType <: TermLike with HasOrigin
    val fieldType: Type
    val node: EntityFieldNode
    val term: TermType
    val nameTerm: TermLike with HasOrigin
  }

  object EntityFieldNodeData{
    trait Aux[T <: TermLike with HasOrigin] extends EntityFieldNodeData{
      type TermType = T
    }
  }

  case class DerivedValueNodeData(fieldType: Type, node: EntityFieldNode, term: DerivedAttribute3) extends EntityFieldNodeData.Aux[DerivedAttribute3]{
    override val nameTerm: Lexical = term.id1
  }

  case class AttributeNodeData(fieldType : Type, node: EntityFieldNode, term: Attribute2) extends EntityFieldNodeData.Aux[Attribute2]{
    override val nameTerm: TermLike with HasOrigin = term.id1
  }

  case class RelationNodeData(fieldType: MultiplicityType[EntityType], entityRef: EntityRef1, attributeRef: AttributeRef1, multiplicity: SMultiplicity, node: EntityFieldNode, term: Relation6) extends EntityFieldNodeData.Aux[Relation6]{
    override val nameTerm: TermLike with HasOrigin = attributeRef
  }

  def mutableSCC[V, E[V] <: EdgeLikeIn[V]](graph: Graph[V, E]): Seq[Either[Seq[Graph[V, E]#NodeT], Graph[V, E]#NodeT]] = {
    import scala.collection.mutable.ListBuffer
    class Vector(var node: Graph[V, E]#NodeT, var index: Int, var lowLink: Int, var onStack: Boolean){
      override def toString: String = node.toString()
    }

    val undefined = -1
    var index = 0
    val s = ListBuffer.empty[Vector]
    var sccs = Seq.empty[Seq[Graph[V, E]#NodeT]]
    val vectors = graph.nodes.map(n => new Vector(n, undefined, undefined, false))
    val adjecencyList = vectors.map(v => v -> v.node.outgoing.map(e => vectors.find(_.node == e.to).get)).toMap

    vectors.foreach{v =>
      if(v.index == -1){
        strongConnect(v)
      }
    }

    def strongConnect(v: Vector){
      v.index = index
      v.lowLink = index
      index = index + 1
      v +=: s
      v.onStack = true

      for(w <- adjecencyList(v)){
        if(w.index == undefined){
          strongConnect(w)
          v.lowLink = Math.min(v.lowLink, w.lowLink)
        } else if(w.onStack){
          v.lowLink = Math.min(v.lowLink, w.index)
        }
      }

      if(v.lowLink == v.index){
        var scc = Seq.empty[Graph[V, E]#NodeT]

        var w: Vector = null
        do {
          w = s.remove(0)
          w.onStack = false
          scc = scc ++ Seq(w.node)
        } while(w != v)
        sccs = sccs :+ scc
      }
    }
    ///resolve cycles of length 1
    sccs.map(scc => if(scc.length == 1){
      val v = scc.head
      v.outgoing.find(e => e.to == v).map(_ => Left(Seq(v))).getOrElse(Right(v))
    } else {
      Left(scc)
    })
  }
}
