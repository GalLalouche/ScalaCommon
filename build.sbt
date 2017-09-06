lazy val commonSettings = Seq(
  organization := "org.me",
  version := "1.0",
  isSnapshot := true,
  crossScalaVersions := Seq("2.11.11", "2.12.3")
)

lazy val scalaCommon = (project in file("."))
    .settings(commonSettings: _*)
    .settings(
      name := "scalacommon",
      libraryDependencies ++= Seq(
        "com.typesafe.slick" %% "slick" % "3.2.1",
        "io.reactivex" %% "rxscala" % "0.26.4",
        "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
        "org.scalatest" %% "scalatest" % "3.0.4",
        "org.scalaz" %% "scalaz-core" % "7.2.15"
      ))
    .settings(scalacOptions in(Compile, doc) ++= Seq(
      "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
    ))

