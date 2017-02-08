package org.metaborg.entitylang.graph.webservice

import akka.actor.{Actor, Props}
import org.metaborg.entitylang.analysis.AnalysisGraph
import org.metaborg.entitylang.graph.Graph
import org.metaborg.entitylang.graph.webservice.GraphWebServiceActor.GraphPushed

class LatestGraphActor extends Actor {
  import LatestGraphActor._

  var latest : Option[AnalysisGraph] = None

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[GraphPushed])
  }

  override def receive: Receive = {
    case GraphPushed(graph) => latest = Some(graph)
    case GetLatest() =>
      val s = sender()
      latest.foreach(g => s ! Latest(g))
  }
}


object LatestGraphActor{
  def props() = Props[LatestGraphActor]

  case class GetLatest()
  case class Latest(graph: AnalysisGraph)
}
