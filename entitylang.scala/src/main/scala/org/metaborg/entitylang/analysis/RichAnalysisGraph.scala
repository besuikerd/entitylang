package org.metaborg.entitylang.analysis

import scala.reflect.{ClassTag, classTag}
import scalax.collection.GraphTraversal.DepthFirst


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


  def scc = {
    type SCSS = scala.collection.mutable.Map[AnalysisNode, AnalysisNode]
    def dfs(node: AnalysisNode, path: Map[AnalysisNode, Int], scss: SCSS): SCSS = {

      def shallowerNode(old: AnalysisNode, candidate: AnalysisNode): AnalysisNode =
        (path.get(old), path.get(candidate)) match{
          case (_, None) => old
          case (None, _) => candidate
          case (Some(dOld), Some(dCand)) => if(dCand < dOld) candidate else old
        }

      val children = node.diSuccessors
      val (scss2, shallowestBackNode) = children.foldLeft((scss, node)){
        case ((scss, shallowest), child) =>
          if(path.contains(child))
            (scss, shallowerNode(shallowest, child))
          else {
            val sccWithChildData = dfs(child, path + (node -> path.size), scss)
            val shallowestForChild = sccWithChildData(child)
            (sccWithChildData, shallowerNode(shallowest, shallowestForChild))
          }
      }
      scss2 + (node -> shallowestBackNode)
    }

    val scss = graph.entityFields.foldLeft[SCSS](scala.collection.mutable.LinkedHashMap.empty){ case (scss, n) =>
      if(scss.contains(n)){
        scss
      } else {
        dfs(n, Map.empty, scss)
      }
    }

    scss.values.toSeq.distinct

//    graph.entities.map(n => n.innerNodeTraverser.toSeq.size).sum
  }
}
