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
}
