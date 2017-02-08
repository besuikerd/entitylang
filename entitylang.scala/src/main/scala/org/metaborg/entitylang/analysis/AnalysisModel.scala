package org.metaborg.entitylang.analysis

import scalax.collection.Graph

case class AnalysisModel(
  graph: AnalysisGraph,
  data: NodeDataMap.HMapType
) {


  def foldWith[A](s: TraversableOnce[A])(f: AnalysisModel => A => AnalysisModel) : AnalysisModel = s.foldLeft(this){
    case (model, a) => f(model)(a)
  }


  def getEntityFieldData(e: EntityFieldNode): Option[EntityFieldNodeData] = e match {
    case n : DerivedValueNode => data.get(n)
    case n : AttributeNode => data.get(n)
    case n : RelationNode => data.get(n)
  }

//  def addField()
}

object AnalysisModel {
  val empty = AnalysisModel(Graph.empty, NodeDataMap.empty)
}
