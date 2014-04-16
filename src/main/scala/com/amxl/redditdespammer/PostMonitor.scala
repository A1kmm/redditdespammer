package com.amxl.redditdespammer

import akka.actor.{Props, Actor, ActorRef}

class PostMonitor(redditClient: ActorRef, manageStore: ActorRef, subreddit: String) extends Actor {
  def receive: Actor.Receive = {
    case r : RedditPost => context.actorOf(Props(classOf[PostHandler], manageStore, r, subreddit))
  }
}