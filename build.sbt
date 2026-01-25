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

def isTestCode(path: String): Boolean = path.replace("\\", "/").contains("common/test/")

lazy val artifactName = "scalacommon"
lazy val testArtifactName = artifactName + "-test"

lazy val testUtilJar = taskKey[File]("JAR containing only common/test/ classes")
lazy val publishTestLocal = taskKey[Unit](
  "Publish the test-utilities JAR to .ivy2/local as a separate module (org.me:scalacommon-test)",
)

lazy val scalaCommon = (project in file("."))
  .settings(
    name := artifactName,
    Compile / packageBin / mappings := {
      (Compile / packageBin / mappings).value.filterNot { case (_, path) =>
        path.replace('\\', '/').contains("common/test/")
      }
    },
    testUtilJar := {
      val base = (Compile / products).value.head
      val pathFinder = base ** "*"
      val mappings = pathFinder.get.flatMap { file =>
        val path = IO.relativize(base, file).get.replace('\\', '/')
        if (path.contains("common/test/")) Some(file -> path) else None
      }
      val out = (Compile / target).value / (testArtifactName + ".jar")
      sbt.io.IO.jar(mappings, out, new java.util.jar.Manifest(), Some(0L))
      out
    },
  )
  .settings(
    Seq(
      organization := "org.me",
      version := "2.1",
      isSnapshot := true,
      scalaVersion := mainVersion, // Needed for IntelliJ, sbt compile (as opposed to sbt +compile), etc.
      crossScalaVersions := Seq(version212, version213),
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
          "org.mockito" % "mockito-core" % "5.15.2" % Provided, // Not in test scope due to AuxSpecs

          "com.h2database" % "h2" % "1.4.196" % Test,
          "org.slf4j" % "slf4j-nop" % "1.6.4" % Test,
          "org.scalatestplus" %% "mockito-3-4" % "3.2.10.0" % Test,
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
      Compile / doc / scalacOptions ++= Vector(
        "-no-link-warnings", // Suppresses problems with Scaladoc @throws links
      ),
    ),
  )
  .settings(
    publishTestLocal := {
      val org = organization.value
      val ver = version.value
      val mod = s"${testArtifactName}_${scalaBinaryVersion.value}"
      val base = file(s"${(file(sys.props("user.home")) / ".ivy2" / "local").getAbsolutePath
          .replace("\\", "/")}/$org/$mod/$ver")
      IO.createDirectory(base / "jars")
      IO.createDirectory(base / "ivys")
      IO.copyFile(
        testUtilJar.value,
        base / "jars" / s"${testArtifactName}_${scalaBinaryVersion.value}.jar",
      )
      IO.write(
        base / "ivys" / s"ivy.xml",
        s"""<?xml version="1.0" encoding="UTF-8"?>
          |<ivy-module version="2.0" xmlns:m="http://ant.apache.org/ivy/maven">
          |  <info organisation="$org" module="$mod" revision="$ver"/>
          |  <publications>
          |    <artifact name="${testArtifactName}_${scalaBinaryVersion.value}" type="jar" ext="jar"/>
          |  </publications>
          |</ivy-module>""".stripMargin,
      )
      streams.value.log.info(s"published $testArtifactName to $base")
    },
  )
