package com.amxl.redditdespammer

import akka.actor.{Props, Actor, ActorRef}

class PostMonitor(redditClient: ActorRef, manageStore: ActorRef, bot: String, subreddit: String) extends Actor {
  def receive: Actor.Receive = {
    case SubredditRemoved(_) =>
      context.stop(self)
    case r : RedditPost => context.actorOf(Props(classOf[PostHandler], sender(),
                              manageStore, redditClient, r, bot, subreddit))
  }
}