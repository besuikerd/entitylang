package org.metaborg.entitylang.analysis

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeLikeIn
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


  /**
    * calculates the set of strongly connected components
    * based off http://stackoverflow.com/questions/15877819/functional-implementation-of-tarjans-strongly-connected-components-algorithm
    * @return sequence of components; either a cycle or a single node
    */
  def stronglyConnectedComponents: Seq[Either[Seq[AnalysisNode], AnalysisNode]] = {
    type SCSS = scala.collection.immutable.Map[AnalysisNode, AnalysisNode]
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

    //TODO ListMap is rather inefficient, but insertion order needs to be preserved
    val scss = graph.entityFields.foldLeft[SCSS](ListMap.empty){ case (scss, n) =>
      if(scss.contains(n)){
        scss
      } else {
        dfs(n, ListMap.empty, scss)
      }
    }
    val representatives = scss.values.toList.distinct
    representatives.map {
      n => {
        //graph library fails to find cycles of size 1 so manual check is needed
        val singleNodeCycle = n.outgoing.find(e => e.to == n).map(_ => Left(Seq(n)))
        singleNodeCycle.getOrElse(
          n.innerNodeTraverser.findCycle.flatMap{
            cycle => {
              val nodes = cycle.nodes.toList.distinct
              if(nodes.contains(n)){
                Some(Left(nodes))
              } else {
                None
              }
            }
          }.getOrElse(Right(n))
        )

      }
    }
  }
}
