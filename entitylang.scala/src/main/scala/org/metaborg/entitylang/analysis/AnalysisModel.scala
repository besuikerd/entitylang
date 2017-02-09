package org.metaborg.entitylang.analysis

import scalax.collection.Graph

case class AnalysisModel(
  graph: AnalysisGraph,
  fields: Map[EntityFieldNode, EntityFieldNodeData],
  entities: Map[EntityNode, EntityNodeData]
) {


  def foldWith[A](s: TraversableOnce[A])(f: AnalysisModel => A => AnalysisModel) : AnalysisModel = s.foldLeft(this){
    case (model, a) => f(model)(a)
  }
}

object AnalysisModel {
  val empty: AnalysisModel = AnalysisModel(Graph.empty, Map.empty, Map.empty)
}
