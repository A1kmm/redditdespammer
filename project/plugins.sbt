// addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.1")
Seq[Setting[_]](resolvers += "artifactory" at "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases",
                addSbtPlugin("com.eed3si9n" %% "sbt-assembly" % "0.9.2"))
