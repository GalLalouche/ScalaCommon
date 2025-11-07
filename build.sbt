val GuiceVersion = "4.2.3"
val version212 = "2.12.20"
val version213 = "2.13.16"
val mainVersion = version213
val catsVersion = "2.13.0"
Compile / sourceDirectories += baseDirectory.value / "src/main/scala"
Compile / sourceDirectories += {
  if (scalaVersion.value.startsWith("2.12"))
    baseDirectory.value / "src/main/scala-2.12"
  else if (scalaVersion.value.startsWith("2.13"))
    baseDirectory.value / "src/main/scala-2.13"
  else
    throw new RuntimeException("Unsupported Scala version")
}
Test / sourceDirectories += {
  if (scalaVersion.value.startsWith("2.12"))
    baseDirectory.value / "src/test/scala-2.12"
  else if (scalaVersion.value.startsWith("2.13"))
    baseDirectory.value / "src/test/scala-2.13"
  else
    throw new RuntimeException("Unsupported Scala version")
}

lazy val scalaCommon = (project in file("."))
  .settings(
    organization := "org.me",
    version := "2.0",
    isSnapshot := true,
    name := "scalacommon",
    scalaVersion := mainVersion, // Needed for IntelliJ, sbt compile (as opposed to sbt +compile), etc.
    crossScalaVersions := Seq(version212, version213),
    // TODO temporary fix, until I update other projects to newer scalaz.
    libraryDependencies ++= {
      def versionMap(for212: String, for313: String): String =
        if (scalaVersion.value.startsWith("2.12"))
          for212
        else if (scalaVersion.value.startsWith("2.13"))
          for313
        else
          throw new RuntimeException("Unsupported Scala version")
      Seq(
        "org.typelevel" %% "cats-core" % catsVersion,
        // The below are provided to avoid pulling them in unless explicitly needed in other projects.
        "org.scalaz" %% "scalaz-core" % versionMap("7.2.15", "7.3.8") % Provided,
        "org.typelevel" %% "alleycats-core" % catsVersion % Provided,
        "com.beachape" %% "enumeratum" % "1.5.13",
        "com.github.julien-truffaut" %% "monocle-core" % versionMap("1.5.0", "1.7.3") % Provided,
        "com.github.pathikrit" %% "better-files" % "3.9.2" % Provided,
        "com.google.inject" % "guice" % GuiceVersion % Provided,
        "com.google.inject.extensions" % "guice-assistedinject" % GuiceVersion % Provided,
        "com.typesafe.slick" %% "slick" % "3.3.3" % Provided,
        "io.reactivex" %% "rxscala" % "0.27.0" % Provided,
        "net.codingwell" %% "scala-guice" % "4.2.11" % Provided,
        "org.scalafx" %% "scalafx" % "17.0.1-R26" % Provided,
        "org.scalacheck" %% "scalacheck" % "1.14.3" % Provided, // Not in test scope due to AuxSpecs
        "org.scalatest" %% "scalatest" % "3.2.19" % Provided, // Not in test scope due to AuxSpecs

        "com.h2database" % "h2" % "1.4.196" % Test,
        "org.slf4j" % "slf4j-nop" % "1.6.4" % Test,
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
        // Laws dependencies
        "org.typelevel" %% "cats-laws" % "2.13.0" % Test,
        "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test,
      )
    },
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)),
    scalacOptions ++= (if (scalaVersion.value.startsWith("2.12")) Seq("-Ypartial-unification")
                       else Nil),
  )
  .settings(
    Compile / doc / scalacOptions ++= Vector(
      "-no-link-warnings", // Suppresses problems with Scaladoc @throws links
    ),
  )
