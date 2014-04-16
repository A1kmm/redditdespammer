package com.amxl.redditdespammer

import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import org.json4s._
import spray.http.HttpMethods
import scala.Some
import scala.concurrent.duration._
import scala.language.postfixOps

class SubredditMonitor(redditClient: ActorRef, postMonitor: ActorRef, subreddit: String) extends Actor {
  var seenUpTo : Option[String] = None
  val log = Logging(context.system, this)
  implicit val dispatcher = context.dispatcher
  implicit val formats = DefaultFormats

  private case class RefreshNow()
  context.system.scheduler.schedule(0 second, 10 second, self, RefreshNow())
  def receive: Actor.Receive = {
    case RefreshNow() =>
      val limit = Seq("limit" -> "100")
      val params : Seq[(String, String)]= limit ++ (seenUpTo match {
        case Some(after) => Seq("after" -> after)
        case _ => Seq()
      })
      redditClient ! SendRequest("/r/" + subreddit + "/new.json", HttpMethods.GET, params)
    case RedditResponse(value) =>
      (value \\ "data" \\ "children").asInstanceOf[JArray].arr.foreach(item => {
        val post = (item \\ "data").extract[RedditPost]
        seenUpTo = Some(post.name)
        if (!post.hidden && (post.approved_by == null || post.approved_by.length == 0))
          postMonitor ! post
      })
    case RedditFailure(msg) =>
      log.warning("Reddit Error refreshing: {}", msg)
  }
}
