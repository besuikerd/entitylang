package org.metaborg.entitylang.graph.webservice

import akka.actor.{Actor, Props}
import org.metaborg.entitylang.analysis.AnalysisModel
import org.metaborg.entitylang.graph.webservice.GraphWebServiceActor.ModelPushed

class LatestModelActor extends Actor {
  import LatestModelActor._

  var latest : Option[AnalysisModel] = None

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[ModelPushed])
  }

  override def receive: Receive = {
    case ModelPushed(model) => latest = Some(model)
    case GetLatest() =>
      val s = sender()
      latest.foreach(g => s ! Latest(g))
  }
}


object LatestModelActor{
  def props() = Props[LatestModelActor]

  case class GetLatest()
  case class Latest(model: AnalysisModel)
}
