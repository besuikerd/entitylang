package org.metaborg.entitylang.graph.webservice

import akka.actor.{ActorRef, Props}
import akka.stream.actor.ActorPublisher
import akka.stream.actor.ActorPublisherMessage.Request
import org.metaborg.entitylang.analysis.{AnalysisGraph, AnalysisModel}
import org.metaborg.entitylang.graph.webservice.GraphWebServiceActor.ModelPushed
import org.metaborg.entitylang.graph.webservice.LatestModelActor.{GetLatest, Latest}

import scalax.collection.Graph

class GraphSourceActor(val latest: ActorRef) extends ActorPublisher[AnalysisModel] {

  private var buffer: Option[AnalysisModel] = None

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    latest ! GetLatest()
    context.system.eventStream.subscribe(self, classOf[ModelPushed])
  }

  override def receive: Receive = {
    case ModelPushed(model) if isActive && totalDemand > 0 => {
      onNext(model)
    }
    case Latest(model) => {
      if (totalDemand > 0) {
        onNext(model)
      } else {
        buffer = Some(model)
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