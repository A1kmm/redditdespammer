package com.amxl.redditdespammer

import akka.actor._
import spray.can.Http
import spray.routing.HttpService
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson._
import spray.http.HttpRequest
import spray.http.HttpResponse
import org.json4s.ShortTypeHints
import akka.persistence.Persistent
import akka.io.IO

class ManageConsole(val adminCommandPersister : ActorRef) extends Actor with HttpService {
  implicit val system = context.system
  implicit val formats = new DefaultFormats {
    override val typeHints = ShortTypeHints(
      CommandInformation.userAccesibleQueryTypes ++ CommandInformation.userAccessibleCommandTypes
    )
    override val typeHintFieldName = "type"
  }

  override def preStart() = IO(Http) ! Http.Bind(self, interface = "0.0.0.0", port = Main.adminPort)

  def receive: Actor.Receive = {
    case Http.Connected(_, _) => sender ! Http.Register(self)
    case r : HttpRequest => runRoute(routes).apply(r)
  }

  val routes = {
    path("api") {
      post {
        path("command") { ctx =>
          val command = Serialization.read[Command](ctx.request.entity.asString)
          context.actorOf(Props(new Actor {
            override def preStart() = adminCommandPersister ! PrecheckCommand(command)
            override def receive: Actor.Receive = {
              case CommandSucceeded =>
                ctx.responder ! HttpResponse(200, Serialization.write("success" -> true))
                adminCommandPersister ! Persistent(command)
              case CommandFailed(msg) =>
                ctx.responder ! HttpResponse(400, Serialization.write(pair2Assoc("success" -> false) ~ ("message" -> msg)))
            }
          }))
        } ~ path("query") { ctx =>
          val query = Serialization.read[Query](ctx.request.entity.asString)
          context.actorOf(Props(new Actor {
            override def preStart() = adminCommandPersister ! CheckCredentials(query.sourceUsername, query.password)
            override def receive: Actor.Receive = {
              case CredentialsValid => adminCommandPersister ! query
              case x : CommandResponse => sender() ! HttpResponse(200, Serialization.write(x))
            }
          }))
        }
      }
    } ~ get {
      pathEndOrSingleSlash(getFromResource("public/index.html")) ~
      getFromResourceDirectory("public")
    }
  }

  override def actorRefFactory: ActorRefFactory = context
}
