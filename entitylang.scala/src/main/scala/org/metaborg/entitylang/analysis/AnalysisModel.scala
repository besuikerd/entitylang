package org.metaborg.entitylang.analysis

import org.metaborg.scalaterms.{Origin, STerm}
import org.metaborg.scalaterms.spoofax.EditorMessage
import org.metaborg.entitylang.analysis.types._

import scalax.collection.Graph

case class AnalysisModel(
  graph: AnalysisGraph,
  fields: Map[EntityFieldNode, EntityFieldNodeData],
  entities: Map[EntityNode, EntityNodeData],
  errors: Seq[EditorMessage] = Seq.empty
) {

  def foldWith[A](s: TraversableOnce[A])(f: AnalysisModel => A => AnalysisModel) : AnalysisModel = s.foldLeft(this){
    case (model, a) => f(model)(a)
  }

  def entityScope(entityName: String): Map[String, Type] =
    fields.values.filter(f => f.node.entity == entityName).map(f => f.node.name -> f.fieldType).toMap

  def reportError(message: String, origin: Origin): AnalysisModel = copy(errors = EditorMessage(message, origin) +: errors)
}

object AnalysisModel {
  val empty: AnalysisModel = AnalysisModel(Graph.empty, Map.empty, Map.empty)
}
