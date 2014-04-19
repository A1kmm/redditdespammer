package com.amxl.redditdespammer

import akka.actor._
import spray.can.Http
import spray.routing.HttpService
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson._
import akka.persistence.Persistent
import akka.io.IO
import spray.http.HttpRequest
import scala.Some
import spray.http.HttpResponse
import org.json4s.ShortTypeHints

class ManageConsole(val adminCommandPersister : ActorRef, val sessionTracker : ActorRef)
  extends Actor with HttpService {
  implicit val system = context.system
  implicit val formats = new DefaultFormats {
    override val typeHints = ShortTypeHints(
      CommandInformation.userAccesibleQueryTypes ++ CommandInformation.userAccessibleCommandTypes ++
      CommandInformation.responseTypes
    )
    override val typeHintFieldName = "type"
  }

  override def preStart() = IO(Http) ! Http.Bind(self, interface = "0.0.0.0", port = Main.adminPort)

  def receive: Actor.Receive = {
    case Http.Connected(_, _) => sender ! Http.Register(self)
    case r : HttpRequest => runRoute(routes).apply(r)
  }

  val routes = {
    pathPrefix("api") {
      path("command") { post { ctx =>
        val httpSender = sender()
        parseJson(ctx.request.entity.asString).extractOpt[Command] match {
          case Some(command) =>
            sessionTracker ! RenewSession(command.sessionId)
            context.actorOf(Props(new Actor {
              override def preStart() = adminCommandPersister ! PrecheckCommand(command)
              override def receive: Actor.Receive = {
                case CommandSucceeded(toPersist) =>
                  ctx.responder ! HttpResponse(200, Serialization.write("success" -> true))
                  adminCommandPersister ! Persistent(toPersist)
                  context.stop(self)
                case CommandFailed(msg) =>
                  ctx.responder ! HttpResponse(200, Serialization.write(pair2Assoc("success" -> false) ~ ("message" -> msg)))
                  context.stop(self)
              }
            }))
          case None => ctx.responder ! HttpResponse(400, Serialization.write(pair2Assoc("success" -> false) ~ ("message" -> "Bad JSON")))
      }}} ~ path("query") { post { ctx =>
        parseJson(ctx.request.entity.asString).extractOpt[Query] match {
          case Some(query) =>
            sessionTracker ! RenewSession(query.sessionId)
            context.actorOf(Props(new Actor {
                override def preStart() = adminCommandPersister ! query
                override def receive: Actor.Receive = {
                  case x : CommandResponse =>
                    ctx.responder ! HttpResponse(200, Serialization.write(x))
                    context.stop(self)
                }
              }))
          case None => ctx.responder ! HttpResponse(400, Serialization.write(pair2Assoc("success" -> false) ~ ("message" -> "Bad JSON")))
      }}} ~ path("login") { post { ctx => parseJson(ctx.request.entity.asString).extractOpt[CreateSession] match {
          case Some(cmd) =>
            context.actorOf(Props(new Actor {
              override def preStart() = sessionTracker ! cmd
              override def receive: Actor.Receive = {
                case x : CommandResponse =>
                  ctx.responder ! HttpResponse(200, Serialization.write(x))
                  context.stop(self)
              }
            }))
          case _ => ctx.responder ! HttpResponse(400, Serialization.write(pair2Assoc("success" -> false) ~ ("message" -> "Bad JSON")))
        }
      }}
    } ~ get {
      pathEndOrSingleSlash(getFromResource("public/index.html")) ~
      getFromResourceDirectory("public")
    }
  }

  override def actorRefFactory: ActorRefFactory = context
}
