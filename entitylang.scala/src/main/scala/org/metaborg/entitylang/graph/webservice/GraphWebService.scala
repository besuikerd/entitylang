package org.metaborg.entitylang.graph.webservice

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.metaborg.entitylang.analysis.{AnalysisGraph, AnalysisModel}
import org.metaborg.entitylang.graph.Graph
import org.metaborg.entitylang.graph.webservice.GraphWebServiceActor.{ModelPushed, Start, Stop}
import org.strategoxt.lang.Context

object GraphWebService{


  val config = ConfigFactory.defaultApplication()

  implicit val system = ActorSystem(
    name = "graph-webservice",
    config = config,
    classLoader = getClass.getClassLoader
  )
  implicit val executionContext = system.dispatcher

  val latest = system.actorOf(LatestModelActor.props())
  val webService = system.actorOf(GraphWebServiceActor.props(latest))

  def pushModel(graph: AnalysisModel): Unit ={
    system.eventStream.publish(ModelPushed(graph))
  }

  def startService(ctx: Context): Unit = {
    ctx.getIOAgent.printError("Starting graph service on http://localhost:8080/graph")
    webService ! Start("localhost", 8080)
  }

  def stopService(ctx: Context): Unit = {
    ctx.getIOAgent.printError("Killing graph service...")
    webService ! Stop()
  }
}

