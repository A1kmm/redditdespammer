package com.amxl.redditdespammer

abstract sealed class Command {
  val sessionId: String
}

case class GrantAccess(sessionId: String, username: String, bot: String, subreddit: String, permission: Permission) extends Command
case class RevokeAccess(sessionId: String, username: String, bot: String, subreddit: String, permission: Permission) extends Command
case class AddUser(sessionId: String, username: String, password: String) extends Command
case class SetPassword(sessionId: String, username: String, password: String) extends Command
case class BlockUser(sessionId: String, username: String) extends Command
case class UnblockUser(sessionId: String, username: String) extends Command
case class AddBot(sessionId: String, bot: String, redditUsername: String, redditPassword: String) extends Command
case class DeleteBot(sessionId: String, bot: String) extends Command
case class AddSubreddit(sessionId: String, bot: String, subreddit: String) extends Command
case class DeleteSubreddit(sessionId: String, bot: String, subreddit: String) extends Command
case class AddExemptUser(sessionId: String, bot: String, subreddit: String, username: String) extends Command
case class DeleteExemptUser(sessionId: String, bot: String, subreddit: String, username: String) extends Command
case class AddBannedPhrase(sessionId: String, bot: String, subreddit: String, phrase: String) extends Command
case class DeleteBannedPhrase(sessionId: String, bot: String, subreddit: String, phrase: String) extends Command
// Don't forget to add new classes to the CommandInfo object.

abstract sealed class Query {
  val sessionId: String
}
case class ShowUsersAndAccess(sessionId: String, bot: String, subreddit: String) extends Query
case class ShowAccessForUser(sessionId: String, user: String) extends Query
case class ListBots(sessionId: String) extends Query
case class ListSubreddits(sessionId: String, bot: String) extends Query
case class ListExemptUsers(sessionId: String, bot: String, subreddit: String) extends Query
case class ListBannedPhrases(sessionId: String, bot: String, subreddit: String) extends Query
// Don't forget to add new classes to the CommandInfo object.

sealed class Permission
case class GrantAccessPermission() extends Permission
case class RevokeAccessPermission() extends Permission
case class AddUserPermission() extends Permission
case class SetPasswordPermission() extends Permission
case class BlockUserPermission() extends Permission
case class UnblockUserPermission() extends Permission
case class AddBotPermission() extends Permission
case class DeleteBotPermission() extends Permission
case class AddSubredditPermission() extends Permission
case class DeleteSubredditPermission() extends Permission
case class AddExemptUserPermission() extends Permission
case class DeleteExemptUserPermission() extends Permission
case class AddBannedPhrasePermission() extends Permission
case class DeleteBannedPhrasePermission() extends Permission

case class ShowUsersAndAccessPermission() extends Permission
case class ListBotsPermission() extends Permission
case class ListSubredditsPermission() extends Permission
case class ListExemptUsersPermission() extends Permission
case class ListBannedPhrasesPermission() extends Permission
case class ShowAccessForUserPermission() extends Permission
// Don't forget to add new classes to the CommandInfo object.

case class PermissionForUser(permission: String, bot: String, subreddit: String)
case class PermissionForSubreddit(username: String, permission: String)

sealed abstract class CommandResponse
case class CommandSucceeded(followupWith: Command) extends CommandResponse
case class CommandFailed(msg: String) extends CommandResponse
case class CredentialsValid()
case class PermissionsResult(permissions: List[PermissionForSubreddit],
                             possiblePermissions: List[String]) extends CommandResponse
case class PermissionsForUserResult(username: String, permissions: List[PermissionForUser],
                                    status: String, possiblePermissions: List[String]) extends CommandResponse
case class BotsResult(bots: List[String]) extends CommandResponse
case class SubredditsResult(subreddits: List[String]) extends CommandResponse
case class ExemptUsersResult(users: List[String]) extends CommandResponse
case class BannedPhrasesResult(phrases: List[String]) extends CommandResponse
case class SessionStarted(username: String, sessionId: String) extends CommandResponse
// Don't forget to add new classes to the CommandInfo object.

object CommandInfo {
  /*
  def classesOfType[T]: Set[Class[_]] = {
    val mirror = universe.runtimeMirror(classOf[Command].getClassLoader)
    universe.typeOf[Command].typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sealedDescendants
      .map(sym => mirror.runtimeClass(sym.asInstanceOf[universe.Symbol].asClass))
  }

  val commandClasses = classesOfType[Command]
  val queryClasses = classesOfType[Query]
  val permissionClasses = classesOfType[Permission]
  */

  val commandClasses = List(
    classOf[GrantAccess],
    classOf[RevokeAccess],
    classOf[AddUser],
    classOf[SetPassword],
    classOf[BlockUser],
    classOf[UnblockUser],
    classOf[AddBot],
    classOf[DeleteBot],
    classOf[AddSubreddit],
    classOf[DeleteSubreddit],
    classOf[AddExemptUser],
    classOf[DeleteExemptUser],
    classOf[AddBannedPhrase],
    classOf[DeleteBannedPhrase]  
  )
  val queryClasses = List(
    classOf[ShowUsersAndAccess],
    classOf[ShowAccessForUser],
    classOf[ListBots],
    classOf[ListSubreddits],
    classOf[ListExemptUsers],
    classOf[ListBannedPhrases]
  )

  val permissionClasses = List(
    classOf[GrantAccessPermission],
    classOf[RevokeAccessPermission],
    classOf[AddUserPermission],
    classOf[SetPasswordPermission],
    classOf[BlockUserPermission],
    classOf[UnblockUserPermission],
    classOf[AddBotPermission],
    classOf[DeleteBotPermission],
    classOf[AddSubredditPermission],
    classOf[DeleteSubredditPermission],
    classOf[AddExemptUserPermission],
    classOf[DeleteExemptUserPermission],
    classOf[AddBannedPhrasePermission],
    classOf[DeleteBannedPhrasePermission],

    classOf[ShowUsersAndAccessPermission],
    classOf[ListBotsPermission],
    classOf[ListSubredditsPermission],
    classOf[ListExemptUsersPermission],
    classOf[ListBannedPhrasesPermission],
    classOf[ShowAccessForUserPermission]
  )

  val permissionNames = permissionClasses.map(c => c.getSimpleName)
  
  val commandResponseClasses = List(
    classOf[CommandSucceeded],
    classOf[CommandFailed],
    classOf[CredentialsValid],
    classOf[PermissionsResult],
    classOf[PermissionsForUserResult],
    classOf[BotsResult],
    classOf[SubredditsResult],
    classOf[ExemptUsersResult],
    classOf[BannedPhrasesResult],
    classOf[SessionStarted]
  )
}
