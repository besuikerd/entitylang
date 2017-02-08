package org.metaborg.entitylang.graph.webservice

import java.io.File

import akka.actor.ActorRef
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Sink, Source}
import org.metaborg.entitylang.analysis.AnalysisGraph
import scalax.collection.io.json._

trait GraphWebsocketHandler {

  val latestGraphActor: ActorRef

  val wsRoute: Route = { ctx =>
    import akka.http.scaladsl.model.ws._

    ctx.request.header[UpgradeToWebSocket] match{
      case Some(upgrade) => {
        val source: Source[Message, ActorRef] = Source.actorPublisher[AnalysisGraph](GraphSourceActor.props(latestGraphActor)).map(g => TextMessage(g.toJson(AnalysisGraphDescriptor.descriptor)))
        ctx.complete(upgrade.handleMessagesWithSinkSource(Sink.foreach(m => println(m)), source))
      }
      case None => {
        val url = getClass.getResource("/graph.html")
        //        val path = new File(url.toURI).toPath
        val path = new File("/home/nick/projects/spoofax/entitylang/entitylang.scala/src/main/resources/graph.html").toPath
        val html =  scala.io.Source.fromURL(url).getLines().mkString("")
        ctx.complete(
          HttpResponse(
            status = 200,
            entity = HttpEntity.fromPath(ContentTypes.`text/html(UTF-8)`, path)
          )
        )
      }
    }
  }
}
