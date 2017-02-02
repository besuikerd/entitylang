package org.metaborg.entitylang.graph

import spray.json.DefaultJsonProtocol


case class Node(label: String)
case class Edge(from: String, to: String)

case class Graph(nodes: Seq[Node], edges: Seq[Edge])

object GraphJsonFormat extends DefaultJsonProtocol{
  implicit val nodeFormat = jsonFormat1(Node.apply)
  implicit val edgeFormat=  jsonFormat2(Edge.apply)
  implicit val graphFormat = jsonFormat2(Graph.apply)
}