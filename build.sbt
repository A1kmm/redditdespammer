import AssemblyKeys._

name := "redditdespammer"

version := "0.1"

scalaVersion := "2.10.3"

fork := true

scalacOptions ++= Seq("-feature")

libraryDependencies ++= Seq(
  "io.spray" % "spray-can" % "1.3.0",
  "io.spray" % "spray-client" % "1.3.0",
  "io.spray" % "spray-routing" % "1.3.0",
  "com.typesafe" % "config" % "1.2.0",
  "com.typesafe.akka" %% "akka-actor" % "2.3.0",
  "com.typesafe.akka" %% "akka-contrib" % "2.3.0",
  "com.typesafe.akka" %% "akka-persistence-experimental" % "2.3.0",
  "joda-time" % "joda-time" % "2.3",
  "org.json4s" %% "json4s-native" % "3.2.7",
  "org.mindrot" % "jbcrypt" % "0.3m",
  "org.json4s" %% "json4s-jackson" % "3.2.8"
)

resolvers += "spray repo" at "http://repo.spray.io"

assemblySettings
