package org.metaborg.entitylang.analysis

import org.metaborg.scalaterms.{Origin, STerm}
import org.metaborg.scalaterms.spoofax.EditorMessage
import org.metaborg.entitylang.analysis.types._

import scala.reflect.{ClassTag, classTag}
import scalax.collection.Graph

case class AnalysisModel(
  graph: AnalysisGraph,
  fields: Map[EntityFieldNode, EntityFieldNodeData],
  errors: Seq[EditorMessage] = Seq.empty
) {

  def foldWith[A](s: TraversableOnce[A])(f: AnalysisModel => A => AnalysisModel) : AnalysisModel = s.foldLeft(this){
    case (model, a) => f(model)(a)
  }

  def entityScope(entityName: String): Map[String, Type] =
    fields.values.filter(f => f.node.entity == entityName).map(f => f.node.name -> f.fieldType).toMap

  def fieldType(entity: String, field: String): Option[Type] = fields.collectFirst{
    case (k, v) if k.entity == entity && k.name == field => v.fieldType
  }


  def entityFieldOfType[T <: EntityFieldNodeData: ClassTag]: Seq[T] = {
    val cls = classTag[T].runtimeClass.asInstanceOf[Class[T]]
    fields.values.collect{
      case f if cls.isInstance(f) => cls.cast(f)
    }.toSeq
  }

  def derivedValues = entityFieldOfType[DerivedValueNodeData]
  def attributes = entityFieldOfType[AttributeNodeData]
  def relations = entityFieldOfType[RelationNodeData]


  def verifyUniqueField(field: EntityFieldNodeData): AnalysisModel = {
    fields.get(field.node) match{
      case Some(field2) =>
        val errorMessage = s"Duplicate definition of ${field.node.entity}.${field.node.name}"
        reportError(errorMessage, field.nameTerm.origin)
          .reportError(errorMessage, field2.nameTerm.origin)
      case None =>
        this
    }
  }

  def reportError(message: String, origin: Origin): AnalysisModel = copy(errors = EditorMessage(message, origin) +: errors)
}

object AnalysisModel {
  val empty: AnalysisModel = AnalysisModel(Graph.empty, Map.empty)
}
