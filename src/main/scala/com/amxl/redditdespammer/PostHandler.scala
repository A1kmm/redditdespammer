package com.amxl.redditdespammer

import akka.actor.{FSM, Actor, ActorRef}

sealed trait PostState
case object PendingCheckExempt extends PostState
case object PendingCheckURL extends PostState
case object PendingCheckTitle extends PostState
case object PendingCheckComment extends PostState

case class RedditPost(name: String, selftext: String, url: String, title: String, author: String, approved_by: String,
                      hidden: Boolean)

class PostHandler(managementStore: ActorRef, redditPost: RedditPost, subreddit: String) extends Actor with
  FSM[PostState, List[String]] {
  managementStore ! QueryUserExempt(redditPost.author, subreddit)
  startWith(PendingCheckExempt, List())
  when(PendingCheckExempt) {
    case Event(UserExempt, _) => stop()
    case Event(UserNotExempt, _) =>
      managementStore ! CheckString(redditPost.url)
      goto(PendingCheckURL)
  }
  def banCheckAndProceed(errorIn: String, nextString: Option[String], nextState: State) : StateFunction = {
    case Event(StringNotBanned, _) =>
      nextString.map(str => managementStore ! CheckString(str))
      nextState
    case Event(StringBanned(badString), st) =>
      nextString.map(str => managementStore ! CheckString(str))
      nextState using ((errorIn + " contains banned string " + badString)::st)
  }
  when(PendingCheckURL)(banCheckAndProceed("URL", Some(redditPost.title), goto(PendingCheckTitle)))
  when(PendingCheckTitle)(banCheckAndProceed("Title", Some(redditPost.selftext), goto(PendingCheckComment)))
  when(PendingCheckComment)(banCheckAndProceed("Self text", None, stop()))
}
