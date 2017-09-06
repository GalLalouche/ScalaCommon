lazy val commonSettings = Seq(
  organization := "org.me",
  version := "1.0",
  isSnapshot := true,
  scalaVersion := "2.11.8"
)

lazy val scalaCommon = (project in file("."))
    .settings(commonSettings: _*)
    .settings(
      name := "scalacommon",
      libraryDependencies ++= Seq(
        "io.reactivex" %% "rxscala" % "0.26.4",
        "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
        "org.scalatest" %% "scalatest" % "3.0.4",
        "org.scalaz" %% "scalaz-core" % "7.2.15"
      ))
    .settings(scalacOptions in(Compile, doc) ++= Seq(
      "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
    ))

