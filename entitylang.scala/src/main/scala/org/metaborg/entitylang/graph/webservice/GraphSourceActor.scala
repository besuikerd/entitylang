package org.metaborg.entitylang.graph.webservice

import akka.actor.{ActorRef, Props}
import akka.stream.actor.ActorPublisher
import akka.stream.actor.ActorPublisherMessage.Request
import org.metaborg.entitylang.analysis.AnalysisGraph
import org.metaborg.entitylang.graph.webservice.GraphWebServiceActor.GraphPushed
import org.metaborg.entitylang.graph.webservice.LatestGraphActor.{GetLatest, Latest}

import scalax.collection.Graph

class GraphSourceActor(val latest: ActorRef) extends ActorPublisher[AnalysisGraph] {

  private var buffer: Option[AnalysisGraph] = None

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    latest ! GetLatest()
    context.system.eventStream.subscribe(self, classOf[GraphPushed])
  }

  override def receive: Receive = {
    case GraphPushed(graph) if isActive && totalDemand > 0 => {
      onNext(graph)
    }
    case Latest(g) => {
      if (totalDemand > 0) {
        onNext(g)
      } else {
        buffer = Some(g)
      }
    }
    case Request(_) if buffer.nonEmpty => {
      buffer.foreach(onNext)
      buffer = None
    }
  }
}

object GraphSourceActor {
  def props(latest: ActorRef) = Props(classOf[GraphSourceActor], latest)
}