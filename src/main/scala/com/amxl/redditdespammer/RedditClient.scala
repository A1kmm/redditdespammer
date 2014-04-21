package com.amxl.redditdespammer

import akka.actor.{Props, ActorRef, Stash, Actor}
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

import scala.collection.mutable
import java.net.{URLEncoder, URI, URL}

case class SendRequest(callTo: String, method: HttpMethod, params : Seq[(String, String)])

sealed abstract class RedditResult
case class RedditFailure(details: String) extends RedditResult
case class RedditResponse(value: JValue) extends RedditResult

case class LoginSucceeded()
case class LoginNow()

case class LoginData(modhash: String, cookie: String)

case class FixupNext(invalidName: String)

class RedditClient(manageStore: ActorRef, httpStack: ActorRef, botName: String, username: String, password: String)
  extends Actor with Stash {
  val log = Logging(context.system, this)
  private implicit val dispatcher : ExecutionContext = context.dispatcher
  private implicit val formats = DefaultFormats
  private implicit val timeout : akka.util.Timeout = 60.seconds
  private val subredditActors : mutable.Map[String, (ActorRef, ActorRef)] = mutable.Map.empty

  var loginData = LoginData("", "")

  override def preStart(): Unit = {
    context.become(loggingIn)
    self ! LoginNow()
    manageStore ! SubscribeToSubreddits(botName)
  }

  override def receive = {
    case SendRequest(url, method, params) =>
      sendMessage(url, method, params) pipeTo sender
    case SubredditAdded(subredditName) =>
      if (subredditName != "all") {
        val postMonitor = context.actorOf(Props(classOf[PostMonitor], self, manageStore, botName, subredditName))
        val subredditMonitor = context.actorOf(Props(classOf[SubredditMonitor], self, postMonitor, subredditName))
        subredditActors.put(subredditName, (postMonitor, subredditMonitor))
      }
    case r@SubredditRemoved(subredditName) =>
      subredditActors.get(subredditName) foreach { case (postMonitor, subredditMonitor) =>
        postMonitor ! r
        subredditMonitor ! r
      }
      subredditActors.remove(subredditName)
    case BotRemoved(_) =>
      subredditActors.foreach { case (subredditName, (postMonitor, subredditMonitor)) =>
        postMonitor ! SubredditRemoved(subredditName)
        subredditMonitor ! SubredditRemoved(subredditName)
      }
      context.stop(self)
  }

  private def redditRequestToHttpRequest(callTo: String, method: HttpMethod,
                                         params: Seq[(String, String)]): HttpRequest =
    (if (method == HttpMethods.GET)
      (new RequestBuilder(method) : RequestBuilder)(Uri(Main.baseURL + callTo).withQuery(params :_*))
     else
      (new RequestBuilder(method) : RequestBuilder)(Main.baseURL + callTo,
        new FormData(("api_type" -> "json") +: params))) ~>
      addHeader("Cookie", "reddit_session=" + URLEncoder.encode(loginData.cookie, "UTF-8")) ~>
      addHeader("X-Modhash", loginData.modhash)

  private val pipeline =
      (sendReceive(httpStack) : SendReceive) ~>
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
    sendMessage("/api/login", HttpMethods.POST, Seq("user" -> username, "passwd" -> password)).onComplete {
      case Failure(e) => handleFailure(e.toString)
      case Success(RedditFailure(msg)) => handleFailure(msg)
      case Success(RedditResponse(msg)) =>
        log.warning("Login response: {}", msg.toString)
        loginData = (msg \\ "data").extract[LoginData]
        self ! LoginSucceeded()
    }
  }
}
