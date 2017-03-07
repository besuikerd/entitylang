package org.metaborg.entitylang.analysis

import scala.reflect.{ClassTag, classTag}

class RichAnalysisNode(val node: AnalysisNode) extends AnyVal {
  def optTypedValue[T <: AnalysisGraphNode : ClassTag]: Option[T] = {
    val cls = classTag[T].runtimeClass
    if(cls.isInstance(node.value))
      Some(cls.cast(node.value).asInstanceOf[T])
    else
      None
  }

  def typedValue[T <: AnalysisGraphNode : ClassTag]: T = classTag[T].runtimeClass.cast(node.value).asInstanceOf[T]

  def entityOfEntityField: Option[AnalysisNode] =
    node.innerNodeTraverser.findPredecessor(_.value.isInstanceOf[EntityNode])

  def fieldsOfEntity: Seq[AnalysisNode] =
    node.innerNodeTraverser.filter((n: AnalysisNode) => n.value.isInstanceOf[EntityFieldNode]).toList

  def attributesOfEntity: Seq[EntityFieldNode] =
    node.innerNodeTraverser.filter((n: AnalysisNode) => n.value.isInstanceOf[AttributeNode]).map(_.value.asInstanceOf[EntityFieldNode]).toList
}
