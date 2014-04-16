package com.amxl.redditdespammer

import akka.actor.{Stash, Actor}
import akka.event.Logging
import scala.concurrent.{Future, ExecutionContext}
import org.json4s._
import spray.http._
import spray.client.pipelining._
import org.json4s.native.JsonMethods._
import scala.concurrent.duration._
import akka.pattern.pipe
import spray.http.HttpRequest
import scala.util.Failure
import spray.http.HttpResponse
import scala.util.Success
import scala.language.postfixOps

case class SendRequest(callTo: String, method: HttpMethod, params : Seq[(String, String)])

sealed abstract class RedditResult
case class RedditFailure(details: String) extends RedditResult
case class RedditResponse(value: JValue) extends RedditResult

case class LoginSucceeded()
case class LoginNow()

case class LoginData(modhash: String, cookie: String)

class RedditClient extends Actor with Stash {
  val log = Logging(context.system, this)
  private implicit val dispatcher : ExecutionContext = context.dispatcher
  private implicit val formats = DefaultFormats
  private implicit val timeout = 60.seconds

  var loginData = LoginData("", "")

  override def preStart(): Unit = {
    context.become(loggingIn)
    self ! LoginNow()
  }

  override def receive = {
    case SendRequest(url, method, params) =>
      sendMessage(url, method, params) pipeTo sender
  }

  private def redditRequestToHttpRequest(callTo: String, method: HttpMethod,
                                         params: Seq[(String, String)]): HttpRequest = {
    (new RequestBuilder(method) : RequestBuilder)(Main.baseURL + callTo,
      new FormData(("api_type" -> "json") +: params)) ~>
      addHeader("Cookie", loginData.cookie) ~>
      addHeader("X-Modhash", loginData.modhash)
  }

  private val pipeline =
    addHeader("Cookie", loginData.cookie) ~>
      (sendReceive : SendReceive) ~>
      ((response : HttpResponse) => if (response.status.isSuccess)
        RedditResponse(parse(response.entity.asString))
      else
        RedditFailure("Body: " + response.entity.asString + " Status: " + response.status.intValue)
        )

  private def sendMessage(callTo: String, method: HttpMethod, params : Seq[(String, String)]) : Future[RedditResult] =
    pipeline(redditRequestToHttpRequest(callTo, method, params))

  private def loggingIn : Actor.Receive = {
    case LoginSucceeded() =>
      unstashAll()
      context.unbecome()
    case LoginNow() => loginNow()
    case _ => stash()
  }

  private def handleFailure(msg : String) = {
    log.warning("Problem logging in to reddit: {}", msg)
    context.system.scheduler.scheduleOnce(5 seconds, self, LoginNow())
  }

  private def loginNow() = {
    sendMessage("/api/login", HttpMethods.POST, Seq("user" -> Main.username, "passwd" -> Main.password)).onComplete {
      case Failure(e) => handleFailure(e.toString)
      case Success(RedditFailure(msg)) => handleFailure(msg)
      case Success(RedditResponse(msg)) =>
        log.warning("Login response: {}", msg.toString)
        loginData = (msg \\ "data").extract[LoginData]
        self ! LoginSucceeded()
    }
  }
}
