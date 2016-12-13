lazy val commonSettings = Seq(
  organization := "org.me",
  version := "1.0",
  isSnapshot := true,
  scalaVersion := "2.11.8")

lazy val scalaCommon = (project in file("."))
    .settings(commonSettings: _*)
    .settings(
      name := "scalacommon",
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.2.6",
        "org.scalaz" %% "scalaz-core" % "7.2.4",
        "org.scalacheck" %% "scalacheck" % "1.12.1"))
    .settings(scalacOptions in(Compile, doc) ++= Seq(
      "-no-link-warnings" // Suppresses problems with Scaladoc @throws links
    ))
	
