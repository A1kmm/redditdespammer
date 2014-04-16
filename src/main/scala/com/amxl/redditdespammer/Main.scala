package com.amxl.redditdespammer

import java.io.File
import com.typesafe.config._
import akka.actor.{ActorRef, Props, ActorSystem}

/*
class RateLimitScheduler {
  def run[A](f : =>Future[A]): Future[A] =
}
*/

object Main {
  var adminPort : Int = 1234
  var username : String = null
  var password : String = null
  var baseURL : String = null
  var subreddit : String = null

  def main(argv: Array[String]) {
    if (argv.length < 1) {
      System.out.println("Usage: redditdespammer config.properties")
      return
    }

    val config = ConfigFactory.parseFile(new File(argv(0)))
    username = config.getString("reddit.user")
    password = config.getString("reddit.password")
    baseURL = config.getString("reddit.baseURL")
    adminPort = config.getInt("admin.port")
    subreddit = config.getString("reddit.subreddit")

    val actorSystem : ActorSystem = ActorSystem()
    val rootActor : ActorRef = actorSystem.actorOf(Props(classOf[RootActor]))

    rootActor ! Start()
    actorSystem.awaitTermination()
  }

}
