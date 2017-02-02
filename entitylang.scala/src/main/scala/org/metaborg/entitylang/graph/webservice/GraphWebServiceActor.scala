package org.metaborg.entitylang.graph.webservice

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer

import scala.concurrent.Future

class GraphWebServiceActor(val latestGraphActor: ActorRef) extends Actor with GraphWebServiceRoutes{
  import GraphWebServiceActor._
  implicit val materializer = ActorMaterializer()
  implicit val system = context.system
  implicit val executionContext = context.dispatcher

  private var binding: Option[Future[Http.ServerBinding]] = None

  override def receive: Receive = {
    case Start(interface, port) if binding.isEmpty => {
      val bindingFuture = Http().bindAndHandle(Route.handlerFlow(route), interface, port)
      binding = Some(bindingFuture)
    }

    case Stop() => {
      binding.foreach(_.foreach(_.unbind()))
      binding = None
    }
  }

}

object GraphWebServiceActor{
  def props(latest: ActorRef) = Props(classOf[GraphWebServiceActor], latest)
  case class Start(interface: String, port: Int)
  case class Stop()
}
