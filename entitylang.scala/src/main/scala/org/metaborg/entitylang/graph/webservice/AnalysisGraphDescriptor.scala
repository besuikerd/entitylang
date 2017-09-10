package org.metaborg.entitylang.graph.webservice

import org.metaborg.entitylang.analysis._

import scalax.collection.io.json.{Descriptor, NodeDescriptor}
import scalax.collection.io.json.descriptor.predefined.{Di, LkDi}

object AnalysisGraphDescriptor {
  def descriptor[T: Manifest](id: String, f: T => String): NodeDescriptor[T] = new NodeDescriptor[T](typeId = id){
    override def id(node: Any): String = f(node.asInstanceOf[T])
  }

  val entityFieldDescriptor = descriptor[EntityFieldNode]("entityField", n => s"${n.entity}.${n.name}")

  val descriptor = new Descriptor[EntityFieldNode](
    defaultNodeDescriptor = entityFieldDescriptor,
    defaultEdgeDescriptor = LkDi.descriptor("dsa"),
    namedNodeDescriptors = Seq(
      entityFieldDescriptor
    )
  )
}
