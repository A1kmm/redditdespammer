package com.amxl.redditdespammer

import akka.persistence.{Persistent, Processor}
import akka.actor.{Actor, ActorRef}
import scala.collection.mutable
import org.mindrot.jbcrypt.BCrypt

sealed class Permission
case object GrantAccessPermission extends Permission
case object RevokeAccessPermission extends Permission
case object AddUserPermission extends Permission
case object BlockUserPermission extends Permission
case object UnblockUserPermission extends Permission
case object AddBotPermission extends Permission
case object DeleteBotPermission extends Permission
case object AddSubredditPermission extends Permission
case object DeleteSubredditPermission extends Permission
case object AddExemptUserPermission extends Permission
case object DeleteExemptUserPermission extends Permission
case object AddBannedPhrasePermission extends Permission
case object DeleteBannedPhrasePermission extends Permission

case object ShowUsersAndAccessPermission extends Permission
case object ListBotsPermission extends Permission
case object ListSubredditsPermission extends Permission
case object ListExemptUsersPermission extends Permission
case object ListBannedPhrasesPermission extends Permission

abstract sealed class Command {
  val sourceUsername: String
  val password: String
}
case class GrantAccess(sourceUsername: String, password: String, username: String, bot: String, subreddit: String, permission: Permission) extends Command
case class RevokeAccess(sourceUsername: String, password: String, username: String, bot: String, subreddit: String, permission: Permission) extends Command
case class AddUser(sourceUsername: String, password: String, username: String, encryptedPassword: String) extends Command
case class BlockUser(sourceUsername: String, password: String, username: String) extends Command
case class UnblockUser(sourceUsername: String, password: String, username: String) extends Command
case class AddBot(sourceUsername: String, password: String, bot: String, redditUsername: String, redditPassword: String) extends Command
case class DeleteBot(sourceUsername: String, password: String, bot: String) extends Command
case class AddSubreddit(sourceUsername: String, password: String, bot: String, subreddit: String) extends Command
case class DeleteSubreddit(sourceUsername: String, password: String, bot: String, subreddit: String) extends Command
case class AddExemptUser(sourceUsername: String, password: String, bot: String, subreddit: String, username: String) extends Command
case class DeleteExemptUser(sourceUsername: String, password: String, bot: String, subreddit: String, username: String) extends Command
case class AddBannedPhrase(sourceUsername: String, password: String, bot: String, subreddit: String, phrase: String) extends Command
case class DeleteBannedPhrase(sourceUsername: String, password: String, bot: String, subreddit: String, phrase: String) extends Command

abstract sealed class Query {
  val sourceUsername: String
  val password: String
}
case class CheckCredentials(sourceUsername: String, password: String) extends Query
case class ShowUsersAndAccess(sourceUsername: String, password: String, bot: String, subreddit: String) extends Query
case class ListBots(sourceUsername: String, password: String) extends Query
case class ListSubreddits(sourceUsername: String, password: String, bot: String) extends Query
case class ListExemptUsers(sourceUsername: String, password: String, bot: String, subreddit: String) extends Query
case class ListBannedPhrases(sourceUsername: String, password: String, bot: String, subreddit: String) extends Query

object CommandInformation {
  val userAccessibleCommandTypes = List(classOf[GrantAccess], classOf[RevokeAccess], classOf[AddUser],
    classOf[BlockUser], classOf[UnblockUser], classOf[AddBot], classOf[DeleteBot], classOf[AddSubreddit],
    classOf[DeleteSubreddit], classOf[AddExemptUser], classOf[DeleteExemptUser], classOf[AddBannedPhrase],
    classOf[DeleteBannedPhrase])

  val userAccesibleQueryTypes = List(classOf[CheckCredentials], classOf[ShowUsersAndAccess], classOf[ListBots],
    classOf[ListSubreddits], classOf[ListExemptUsers], classOf[ListBannedPhrases])
}

class AdminCommandReceiver(persister: ActorRef) extends Actor {
  def receive: Receive = { case (x: Command) => persister forward Persistent(x) }
}

class CommandResponse
case object CommandSucceeded extends CommandResponse
case class CommandFailed(msg: String) extends CommandResponse
case object CredentialsValid
case class PermissionsResult(permissions: List[Permission]) extends CommandResponse
case class BotsResult(bots: List[String]) extends CommandResponse
case class SubredditsResult(subreddits: List[String]) extends CommandResponse
case class ExemptUsersResult(subreddits: List[String]) extends CommandResponse
case class BannedPhrasesResult(subreddits: List[String]) extends CommandResponse

case class PrecheckCommand(command: Command)

sealed class PermissionScope
case object SubredditAccess extends PermissionScope
case object BotwideAccess extends PermissionScope
case object GlobalAccess extends PermissionScope
case object NoAccess extends PermissionScope

class AdminCommandPersister extends Processor {
  private case class Context(bot: String, subreddit: String)
  private case class UserData(encryptedPassword: String, permissions: mutable.Set[(Context, Permission)],
                               var blocked: Boolean = false)
  private val users: mutable.Map[String, UserData] = mutable.Map()

  private case class SubredditData(exemptUsers: mutable.Set[String], bannedPhrases: mutable.Set[String])
  private case class BotData(redditUsername: String, redditPassword: String,
                             subreddits: mutable.Map[String, SubredditData])
  private val bots: mutable.Map[String, BotData] = mutable.Map("all" -> defaultBotData("", ""))

  private def defaultSubredditData(): SubredditData = SubredditData(mutable.Set(), mutable.Set())
  private def defaultBotData(username: String, pwd: String): BotData = BotData(username, pwd,
    mutable.Map("all" -> defaultSubredditData()))

  private def hasPermission(username: String, bot: String, subreddit: String, permission: Permission) =
    if (username == "root")
      GlobalAccess
    else users.get(username) match {
      case None => NoAccess
      case Some(user) =>
        val permissions = user.permissions
        if (permissions.contains(Context("all", "all"), permission))
          GlobalAccess
        else if (permissions.contains(Context(bot, "all"), permission))
          BotwideAccess
        else if (permissions.contains(Context(bot, subreddit), permission))
          SubredditAccess
        else
          NoAccess
    }

  private def withPrecheckSubreddit(bot: String, subreddit: String, f: (BotData, SubredditData) => Unit) =
    if (bot == "all" && subreddit != "all")
      sender() ! CommandFailed("Only subreddit=all is supported when bot=all")
    else
      bots.get(bot) match {
        case None => sender() ! CommandFailed("No such bot")
        case Some(botData) =>
          botData.subreddits.get(subreddit) match {
            case None => sender() ! CommandFailed("No such subreddit")
            case Some(subredditData) => f(botData, subredditData)
          }
      }

  private def validRedditUsername(redditUsername: String): Boolean =
    redditUsername.contains(' ') || !(redditUsername matches "^[\\w-]{3,20}$")

  def receive = {
    case PrecheckCommand(GrantAccess(sourceUser, _, username, bot, subreddit, permission)) =>
      if (hasPermission(sourceUser, bot, subreddit, GrantAccessPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else users.get(username) match {
        case None => sender() ! CommandFailed("No such target user")
        case Some(user) =>
          if (hasPermission(sourceUser, bot, subreddit, permission) == NoAccess)
            sender() ! CommandFailed("You can't grant a permission you don't have yourself")
          else if (hasPermission(username, bot, subreddit, permission) != NoAccess)
            sender() ! CommandFailed("User already has that permission")
          else
            sender() ! CommandSucceeded
      }
    case Persistent(GrantAccess(sourceUser, _, username, bot, subreddit, permission), _) =>
      users.get(username) match {
        case None => Unit
        case Some(user) => user.permissions.add((Context(bot, subreddit), permission))
      }

    case PrecheckCommand(RevokeAccess(sourceUsername, _, username, bot, subreddit, permission)) =>
      if (hasPermission(sourceUsername, bot, subreddit, RevokeAccessPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (sourceUsername == username)
        sender() ! CommandFailed("You can't modify your own permissions")
      else if (hasPermission(sourceUsername, bot, subreddit, permission) == NoAccess)
        sender() ! CommandFailed("You can't revoke a permission you don't have yourself")
      else if (hasPermission(username, bot, subreddit, permission) == NoAccess)
        sender() ! CommandFailed("User doesn't have that permission")
      else if (bot != "all" && hasPermission(sourceUsername, "all", "all", permission) != NoAccess)
        sender() ! CommandFailed("User has global access to that permission; revoke that first")
      else if ((bot != "all" || subreddit != "all") &&
        hasPermission(sourceUsername, bot, "all", permission) != NoAccess)
        sender() ! CommandFailed("User has bot-level access to that permission; revoke that first")
      else
        sender() ! CommandSucceeded

    case Persistent(RevokeAccess(sourceUsername, _, username, bot, subreddit, permission), _) =>
      users.get(username) match {
        case None => Unit
        case Some(user) => user.permissions.remove((Context(bot, subreddit), permission))
      }

    case PrecheckCommand(AddUser(sourceUsername, _, username, encryptedPassword)) =>
      if (hasPermission(sourceUsername, "all", "all", AddUserPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (username.length > 20)
        sender() ! CommandFailed("Invalid username")
      else if (username == "root")
        sender() ! CommandFailed("Username already taken")
      else users.get(username) match {
        case Some(_) => sender() ! CommandFailed("Username already taken")
        case None => sender() ! CommandSucceeded
      }
    case Persistent(AddUser(sourceUsername, _, username, encryptedPassword), _) =>
      users(username) = UserData(encryptedPassword, mutable.Set())

    case PrecheckCommand(BlockUser(sourceUsername, _, username)) =>
      if (hasPermission(sourceUsername, "all", "all", BlockUserPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else users.get(username) match {
        case None => sender() ! CommandFailed("No such user")
        case Some(UserData(_, _, true)) => CommandFailed("User already blocked")
        case _ => sender() ! CommandSucceeded
      }
    case Persistent(BlockUser(sourceUsername, _, username), _) =>
      users.get(username) match {
        case None => Unit
        case Some(u) => u.blocked = true
      }

    case PrecheckCommand(UnblockUser(sourceUsername, _, username)) =>
      if (hasPermission(sourceUsername, "all", "all", UnblockUserPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else users.get(username) match {
        case None => sender() ! CommandFailed("No such user")
        case Some(UserData(_, _, false)) => CommandFailed("User not blocked")
        case _ => sender() ! CommandSucceeded
      }
    case Persistent(UnblockUser(sourceUsername, _, username), _) =>
      users.get(username) match {
        case None => Unit
        case Some(u) => u.blocked = false
      }

    case PrecheckCommand(AddBot(sourceUsername, _, bot, redditUsername, redditPassword)) =>
      if (hasPermission(sourceUsername, "all", "all", AddBotPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (bots.get(bot).isDefined)
        sender() ! CommandFailed("Bot already exists")
      else if (redditUsername.contains(' ') || !(redditUsername matches "^[\\w-]{3,20}$"))
        sender() ! CommandFailed("Invalid reddit username")
      else
        sender() ! CommandSucceeded
    case Persistent(AddBot(_, _, bot, redditUsername, redditPassword), _) =>
      bots.put(bot, BotData(redditUsername, redditPassword, mutable.Map()))

    case PrecheckCommand(DeleteBot(sourceUsername, _, bot)) =>
      if (hasPermission(sourceUsername, "all", "all", DeleteBotPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (bots.get(bot).isEmpty || bot == "all")
        sender() ! CommandFailed("No such bot")
      else
        sender() ! CommandSucceeded
    case Persistent(DeleteBot(sourceUsername, _, bot), _) =>
      bots.remove(bot)

    case PrecheckCommand(AddSubreddit(sourceUsername, _, bot, subreddit)) =>
      if (hasPermission(sourceUsername, bot, "all", AddSubredditPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (subreddit == "friends" || subreddit == "all" || !subreddit.matches("^[A-Za-z0-9][A-Za-z0-9_]{2,20}$"))
        sender() ! CommandFailed("Invalid subreddit name")
      else bots.get(bot) match {
        case None => sender() ! CommandFailed("No such bot")
        case Some(botData) if botData.subreddits.get(subreddit).isDefined =>
          sender() ! CommandFailed("Subreddit already exists on bot")
        case _ => sender() ! CommandSucceeded
      }
    case Persistent(AddSubreddit(sourceUsername, _, bot, subreddit), _) =>
      bots.get(bot).foreach(botData => botData.subreddits.put(subreddit, defaultSubredditData()))

    case PrecheckCommand(DeleteSubreddit(sourceUsername, _, bot, subreddit)) =>
      if (hasPermission(sourceUsername, bot, "all", DeleteSubredditPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (subreddit == "all")
        sender() ! CommandFailed("No such subreddit")
      else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
        sender() ! CommandSucceeded)
    case Persistent(DeleteSubreddit(sourceUsername, _, bot, subreddit), _) =>
      bots.get(bot).foreach(botData => botData.subreddits.remove(subreddit))

    case PrecheckCommand(AddExemptUser(sourceUsername, _, bot, subreddit, username)) =>
      if (hasPermission(sourceUsername, bot, subreddit, AddExemptUserPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (!validRedditUsername(username))
        sender() ! CommandFailed("Not a valid reddit username")
      else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
        if (subredditData.exemptUsers.contains(username))
          sender() ! CommandFailed("User already exempt")
        else
          sender() ! CommandSucceeded
      )
    case Persistent(AddExemptUser(sourceUsername, _, bot, subreddit, username), _) =>
      bots.get(bot).foreach(botData => botData.subreddits.get(subreddit).foreach(subredditData =>
        subredditData.exemptUsers += username))

    case PrecheckCommand(DeleteExemptUser(sourceUsername, _, bot, subreddit, username)) =>
      if (hasPermission(sourceUsername, bot, subreddit, DeleteExemptUserPermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
        if (subredditData.exemptUsers.contains(username))
          sender() ! CommandSucceeded
        else
          sender() ! CommandFailed("User not exempt")
      )
    case Persistent(DeleteExemptUser(sourceUsername, _, bot, subreddit, username), _) =>
      bots.get(bot).foreach(botData => botData.subreddits.get(subreddit).foreach(subredditData =>
        subredditData.exemptUsers.remove(username)))

    case PrecheckCommand(AddBannedPhrase(sourceUsername, _, bot, subreddit, phrase)) =>
      if (hasPermission(sourceUsername, bot, subreddit, AddBannedPhrasePermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else if (phrase.length > 256)
        sender() ! CommandFailed("Phrase too long")
      else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
        if (subredditData.bannedPhrases.contains(phrase))
          sender() ! CommandFailed("Phrase already banned")
        else
          sender() ! CommandSucceeded
      )
    case Persistent(AddBannedPhrase(sourceUsername, _, bot, subreddit, phrase), _) =>
      bots.get(bot).foreach(botData => botData.subreddits.get(subreddit).foreach(subredditData =>
        subredditData.bannedPhrases += phrase))

    case PrecheckCommand(DeleteBannedPhrase(sourceUsername, _, bot, subreddit, phrase)) =>
      if (hasPermission(sourceUsername, bot, subreddit, DeleteBannedPhrasePermission) == NoAccess)
        sender() ! CommandFailed("Permission denied")
      else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
        if (subredditData.bannedPhrases.contains(phrase))
          sender() ! CommandSucceeded
        else
          sender() ! CommandFailed("Phrase not banned")
      )
    case Persistent(DeleteBannedPhrase(sourceUsername, _, bot, subreddit, phrase), _) =>
      bots.get(bot).foreach(botData => botData.subreddits.get(subreddit).foreach(subredditData =>
        subredditData.bannedPhrases.remove(phrase)))

    case CheckCredentials(sourceUsername, password) =>
      users.get(sourceUsername) match {
        case None => sender() ! CommandFailed("No such user")
        case Some(userData) if userData.blocked => sender() ! CommandFailed("Account disabled")
        case Some(userData) if BCrypt.checkpw(password, userData.encryptedPassword) =>
          sender() ! CommandFailed("Invalid password")
        case _ => sender() ! CredentialsValid
      }

      case ShowUsersAndAccess(sourceUsername, password, bot, subreddit) =>
        if (hasPermission(sourceUsername, bot, subreddit, ShowUsersAndAccessPermission) == NoAccess)
          sender() ! CommandFailed("Permission denied")
        else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
          sender() ! PermissionsResult(users.flatMap { case (username, userData) => userData.permissions
            .filter {
              case (permContext, _) => permContext.bot == bot && permContext.subreddit == subreddit
            }.map { case (_, permission) => permission }}.toList)
        )

      case ListBots(sourceUsername, password) =>
        if (hasPermission(sourceUsername, "all", "all", ListBotsPermission) == NoAccess)
          sender() ! CommandFailed("Permission denied")
        else sender() ! BotsResult(bots.map {
          case (botName, _) => botName
        }.toList)

      case ListSubreddits(sourceUsername, password, bot) =>
        if (hasPermission(sourceUsername, bot, "all", ListSubredditsPermission) == NoAccess)
          sender() ! CommandFailed("Permission denied")
        else if (bot == "all")
          sender() ! CommandFailed("Listing subreddits on all not supported")
        else bots.get(bot) match {
          case None => sender() ! CommandFailed("No such bot")
          case Some(botData) => sender() ! SubredditsResult(botData.subreddits.map {
            case (subreddit, _) => subreddit
          }.toList)
        }

      case ListExemptUsers(sourceUsername, password, bot, subreddit) =>
        if (hasPermission(sourceUsername, bot, subreddit, ListExemptUsersPermission) == NoAccess)
          sender() ! CommandFailed("Permission denied")
        else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
          sender() ! ExemptUsersResult(subredditData.exemptUsers.toList)
        )

      case ListBannedPhrases(sourceUsername, password, bot, subreddit) =>
        if (hasPermission(sourceUsername, bot, subreddit, ListBannedPhrasesPermission) == NoAccess)
          sender() ! CommandFailed("Permission denied")
        else withPrecheckSubreddit(bot, subreddit, (botData, subredditData) =>
          sender() ! ExemptUsersResult(subredditData.exemptUsers.toList)
        )
  }
}
