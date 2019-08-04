val GuiceVersion = "4.2.2"
lazy val scalaCommon = (project in file("."))
    .settings(
      organization := "org.me",
      version := "1.0",
      isSnapshot := true,
      name := "scalacommon",
      scalaVersion := "2.13.0", // Needed for IntelliJ, sbt compile (as opposed to sbt +compile), etc.
      resolvers ++= Vector(
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        "Maven Repository" at "http://repo1.maven.org/maven2/",
        "Apache Snapshot Repository" at "http://repository.apache.org/snapshots/",
        Resolver.file("Local ivy repo", file(System.getProperty("user.home") + "/.ivy2/local"))(Resolver.ivyStylePatterns),
        Resolver.mavenLocal,
      ),
      libraryDependencies ++= Vector(
        "org.scalaz" %% "scalaz-core" % "7.3.0-M31",

        // The below are provided to avoid pulling them in unless explicitly needed in other projects.
        "com.typesafe.slick" %% "slick" % "3.3.2" % "provided",
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "provided", // Not in test scope due to AuxSpecs
        "org.scalatest" %% "scalatest" % "3.0.8" % "provided", // Not in test scope due to AuxSpecs
        "com.google.inject" % "guice" % GuiceVersion % "provided",
        "com.google.inject.extensions" % "guice-assistedinject" % GuiceVersion % "provided",
        "com.github.julien-truffaut" %% "monocle-core" % "1.6.0" % "provided",
        "net.codingwell" %% "scala-guice" % "4.2.6" % "provided",

        "com.h2database" % "h2" % "1.4.196" % "test",
        "org.slf4j" % "slf4j-nop" % "1.6.4" % "test",
      ),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    ).settings(scalacOptions in(Compile, doc) ++= Vector(
  "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
))

