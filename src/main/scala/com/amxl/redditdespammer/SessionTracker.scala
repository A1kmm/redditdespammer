package com.amxl.redditdespammer

import akka.actor.{ActorRef, Props, Actor}
import scala.concurrent.duration._
import scala.collection.mutable
import org.joda.time.DateTime
import java.security.SecureRandom
import org.apache.commons.codec.binary.Base64
import scala.language.postfixOps

case class SessionEnded(sessionId: String)
case class CreateSession(username: String, password: String)
case class RenewSession(sessionId: String)

class SessionTracker(adminCommandPersister: ActorRef) extends Actor {
  private val sessions : mutable.Map[String, Session] = mutable.Map.empty
  private val randomGenerator = new SecureRandom()
  private val sessionTimeout = 1000 * 60 * 10

  private case class Session(expiry: DateTime, username: String)
  private case object ExpireSessions

  private implicit val executionContext = context.dispatcher

  override def preStart() =
    context.system.scheduler.schedule(30 second, 30 second, self, ExpireSessions)

  def receive = {
    case ExpireSessions =>
      val expiredSessions =
        sessions.filter { case (_, Session(d, _)) => d.isBeforeNow }.map { case (sessId, _) => sessId }.toList
      expiredSessions.foreach { case sessId =>
        adminCommandPersister ! SessionEnded(sessId)
        sessions.remove(sessId)
      }
    case CreateSession(username, password) =>
      val originalSender = sender()
      context.actorOf(Props(new Actor {
        override def preStart() =
          adminCommandPersister ! CheckCredentials(username, password)
        def receive: Actor.Receive = {
          case r: CommandFailed => originalSender ! r
          case CredentialsValid =>
            val rawData = new Array[Byte](20)
            randomGenerator.nextBytes(rawData)
            val sessionId = Base64.encodeBase64String(rawData)
            sessions.put(sessionId, Session(DateTime.now().withDurationAdded(sessionTimeout, 1), username))
            adminCommandPersister ! SessionStarted(username, sessionId)
            originalSender ! SessionStarted(username, sessionId)
        }
      }))
    case RenewSession(sessionId: String) =>
      sessions.get(sessionId).foreach(session =>
        sessions.update(sessionId, session.copy(expiry = DateTime.now().withDurationAdded(sessionTimeout, 1))))
  }
}
