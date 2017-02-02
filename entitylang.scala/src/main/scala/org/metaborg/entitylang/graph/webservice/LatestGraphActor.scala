package org.metaborg.entitylang.graph.webservice

import akka.actor.{Actor, Props}
import org.metaborg.entitylang.graph.Graph

class LatestGraphActor extends Actor {
  import LatestGraphActor._

  var latest : Option[Graph] = None

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[Graph])
  }

  override def receive: Receive = {
    case g: Graph => latest = Some(g)
    case GetLatest() =>
      val s = sender()
      latest.foreach(g => s ! Latest(g))
  }
}


object LatestGraphActor{
  def props() = Props[LatestGraphActor]

  case class GetLatest()
  case class Latest(g: Graph)
}
