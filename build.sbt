val GuiceVersion = "4.2.2"
lazy val scalaCommon = (project in file("."))
    .settings(
      organization := "org.me",
      version := "1.0",
      isSnapshot := true,
      name := "scalacommon",
      scalaVersion := "2.12.15", // Needed for IntelliJ, sbt compile (as opposed to sbt +compile), etc.
      crossScalaVersions := Seq("2.11.11", "2.12.15"),
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.2.15",

        // The below are provided to avoid pulling them in unless explicitly needed in other projects.
        "com.beachape" %% "enumeratum" % "1.5.13",
        "com.typesafe.slick" %% "slick" % "3.2.1" % "provided",
        "io.reactivex" %% "rxscala" % "0.26.4" % "provided",
        "org.scalacheck" %% "scalacheck" % "1.13.5" % "provided", // Not in test scope due to AuxSpecs
        "org.scalatest" %% "scalatest" % "3.0.4" % "provided", // Not in test scope due to AuxSpecs
        "com.google.inject" % "guice" % GuiceVersion % "provided",
        "com.google.inject.extensions" % "guice-assistedinject" % GuiceVersion % "provided",
        "com.github.julien-truffaut" %% "monocle-core" % "1.5.0" % "provided",
        "net.codingwell" %% "scala-guice" % "4.2.3" % "provided",

        "com.h2database" % "h2" % "1.4.196" % "test",
        "org.slf4j" % "slf4j-nop" % "1.6.4" % "test",
      ),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
      scalacOptions += "-Ypartial-unification",
    ).settings(scalacOptions in(Compile, doc) ++= Vector(
  "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
))

