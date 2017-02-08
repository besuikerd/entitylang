package org.metaborg.entitylang.analysis

import scala.reflect.{ClassTag, classTag}

class RichAnalysisGraph(val graph: AnalysisGraph) extends AnyVal {
  def findEntity(entityNode: EntityNode): Option[AnalysisNode] = findEntity(entityNode.name)
  def findEntity(name: String): Option[AnalysisNode] = graph.find(EntityNode(name))
  def findEntityField(entity: String, name: String): Option[AnalysisNode] =
    findEntity(entity).flatMap{_.innerNodeTraverser.findSuccessor(
      _.optTypedValue[EntityFieldNode].exists(_.name == name))
    }

  def nodesOfType[T <: AnalysisGraphNode : ClassTag]: Iterable[AnalysisNode] =
    graph.nodes.filter(n => classTag[T].runtimeClass.isInstance(n.value))

  def entities = nodesOfType[EntityNode]
  def entityFields = nodesOfType[EntityFieldNode]
  def derivedValues = nodesOfType[DerivedValueNode]
  def attributes = nodesOfType[AttributeNode]
  def relations = nodesOfType[RelationNode]
}
