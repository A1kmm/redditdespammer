package com.amxl.redditdespammer

import akka.actor.{FSM, Actor, ActorRef}
import spray.http.HttpMethods

sealed trait PostState
case object PendingCheckExempt extends PostState
case object PendingCheckURL extends PostState
case object PendingCheckTitle extends PostState
case object PendingCheckComment extends PostState

case class RedditPost(name: String, selftext: String, url: String, title: String, author: String, approved_by: String,
                      hidden: Boolean)

class PostHandler(subredditMonitor: ActorRef, managementStore: ActorRef, redditClient: ActorRef,
                  redditPost: RedditPost, bot: String, subreddit: String) extends Actor with
  FSM[PostState, List[String]] {
  managementStore ! QueryUserExempt(redditPost.author, bot, subreddit)
  startWith(PendingCheckExempt, List())
  when(PendingCheckExempt) {
    case Event(UserExempt, _) => stop()
    case Event(UserNotExempt, _) =>
      managementStore ! CheckString(redditPost.url, bot, subreddit)
      goto(PendingCheckURL)
  }
  def banCheckAndProceed(errorIn: String, nextString: Option[String], nextState: State) : StateFunction = {
    case Event(StringNotBanned, _) =>
      nextString.map(str => managementStore ! CheckString(str, bot, subreddit))
      nextState
    case Event(StringBanned, st) =>
      redditClient ! SendRequest("/api/remove", HttpMethods.POST, Seq(("id", redditPost.name)))
      subredditMonitor ! FixupNext(redditPost.name)
      stop()
  }
  when(PendingCheckURL)(banCheckAndProceed("URL", Some(redditPost.title), goto(PendingCheckTitle)))
  when(PendingCheckTitle)(banCheckAndProceed("Title", Some(redditPost.selftext), goto(PendingCheckComment)))
  when(PendingCheckComment)(banCheckAndProceed("Self text", None, stop()))
}
