lazy val scalaCommon = (project in file("."))
    .settings(
      organization := "org.me",
      version := "1.0",
      isSnapshot := true,
      name := "scalacommon",
      scalaVersion := "2.12.3", // Needed for IntelliJ, sbt compile (as opposed to sbt +compile), etc.
      crossScalaVersions := Seq("2.11.11", "2.12.3"),
      libraryDependencies ++= Seq(
        "com.typesafe.slick" %% "slick" % "3.2.1",
        "io.reactivex" %% "rxscala" % "0.26.4",
        "org.scalatest" %% "scalatest" % "3.0.4", // Not in test scope due to AuxSpecs
        "org.scalaz" %% "scalaz-core" % "7.2.15",

        "com.h2database" % "h2" % "1.4.196" % "test",
        "org.slf4j" % "slf4j-nop" % "1.6.4" % "test",
        "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
      ))
    .settings(scalacOptions in(Compile, doc) ++= Seq(
      "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
    ))

