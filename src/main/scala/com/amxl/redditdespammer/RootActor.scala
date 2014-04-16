package com.amxl.redditdespammer

import akka.actor._
import akka.contrib.throttle.TimerBasedThrottler
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.Predef._
import scala.Some
import akka.contrib.throttle.Throttler.SetTarget
import akka.contrib.throttle.Throttler.Rate

case class QueryUserExempt(user: String, subreddit: String)
case class CheckString(user: String)

case object UserExempt
case object UserNotExempt

case class StringBanned(badString: String)
case object StringNotBanned

case class Start()
class RootActor extends Actor {
  val manageStore = context.actorOf(Props[AdminCommandPersister], "manageStore")
  val manageConsole = context.actorOf(Props(classOf[ManageConsole], manageStore), "manageConsole")

  val rateLimitedRedditClient = context.actorOf(Props(classOf[TimerBasedThrottler], Rate(5, 10 seconds)),
    "rateLimitedRedditClient")
  val redditClient = context.actorOf(Props[RedditClient], "redditClient")
  rateLimitedRedditClient ! SetTarget(Some(redditClient))
  val postMonitor = context.actorOf(Props(classOf[PostMonitor], rateLimitedRedditClient, manageStore, "blah"),
    "postMonitor")
  val subredditMonitor = context.actorOf(Props(classOf[SubredditMonitor], rateLimitedRedditClient, postMonitor,
    Main.subreddit), "subredditMonitor")

  implicit val theSystem = context.system

  def receive: Actor.Receive = {
    case Start() =>
      System.out.println("Starting up client...")
      manageConsole ! Start()
    case RedditResponse(str) =>
      System.out.println("Reddit response: " + str)
  }
}
