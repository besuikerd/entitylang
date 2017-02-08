package org.metaborg.entitylang.graph.webservice

import org.metaborg.entitylang.analysis._

import scalax.collection.io.json.{Descriptor, NodeDescriptor}
import scalax.collection.io.json.descriptor.predefined.Di

object AnalysisGraphDescriptor {
  def descriptor[T: Manifest](id: String, f: T => String): NodeDescriptor[T] = new NodeDescriptor[T](typeId = id){
    override def id(node: Any): String = f(node.asInstanceOf[T])
  }

  val entityDescriptor = descriptor[EntityNode]("entity", _.name)
  val attributeDescriptor = descriptor[AttributeNode]("attribute", n => s"${n.entity}.${n.name}")
  val derivedValueDescriptor = descriptor[DerivedValueNode]("derivedValue", n => s"${n.entity}.${n.name}")
  val relationDescriptor = descriptor[RelationNode]("relation", n => s"${n.entity}.${n.name}")

  val descriptor = new Descriptor[AnalysisGraphNode](
    defaultNodeDescriptor = entityDescriptor,
    defaultEdgeDescriptor = Di.descriptor(),
    namedNodeDescriptors = Seq(
      attributeDescriptor,
      derivedValueDescriptor,
      relationDescriptor
    )
  )
}
