package org.metaborg.entitylang.graph.webservice

import akka.actor.{ActorRef, Props}
import akka.stream.actor.ActorPublisher
import akka.stream.actor.ActorPublisherMessage.Request
import org.metaborg.entitylang.graph.Graph
import org.metaborg.entitylang.graph.webservice.LatestGraphActor.{GetLatest, Latest}

class GraphSourceActor(val latest: ActorRef) extends ActorPublisher[Graph] {

  private var buffer: Option[Graph] = None

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    latest ! GetLatest()
    context.system.eventStream.subscribe(self, classOf[Graph])
  }

  override def receive: Receive = {
    case g: Graph if isActive && totalDemand > 0 => {
      onNext(g)
    }
    case Latest(g) => {
      if (totalDemand > 0) {
        onNext(g)
      } else {
        buffer = Some(g)
      }
    }
    case Request(_) if buffer.nonEmpty => {
      buffer.foreach(onNext(_))
      buffer = None
    }
  }
}

object GraphSourceActor {
  def props(latest: ActorRef) = Props(classOf[GraphSourceActor], latest)
}