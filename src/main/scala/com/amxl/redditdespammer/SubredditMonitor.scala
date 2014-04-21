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
        case Some(after) => Seq("before" -> after)
        case _ => Seq()
      })
      redditClient ! SendRequest("/r/" + subreddit + "/new.json", HttpMethods.GET, params)
    case RedditResponse(value) =>
      (value \ "data" \ "children").asInstanceOf[JArray].arr.reverse.foreach(item => {
        val post = (item \ "data").extract[RedditPost]
        seenUpTo = Some(post.name)
        if (!post.hidden && (post.approved_by == null || post.approved_by.length == 0))
          postMonitor ! post
      })
    case RedditFailure(msg) =>
      log.warning("Reddit Error refreshing: {}", msg)
    case SubredditRemoved(_) =>
      context.stop(self)
    // This works around a corner case where the last seen post is removed. In this case, reddit will never return any
    // posts in the list after because it treats it as no longer being in the list. In this case, we just start over
    // from the most recent posts.
    case FixupNext(nowInvalidName) =>
      if (Some(nowInvalidName) == seenUpTo)
        seenUpTo = None
  }
}
