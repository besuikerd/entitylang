package org.metaborg.entitylang.graph.webservice

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Directives

trait GraphWebServiceRoutes extends Directives with GraphWebsocketHandler{
  val route =
      path("graph"){
        withoutRequestTimeout{
          get{
            wsRoute
          }
        }
      } ~
      pathPrefix("public"){
        getFromResourceDirectory("public")
      }

}
