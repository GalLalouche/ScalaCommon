lazy val commonSettings = Seq(
	organization := "org.me",
	version := "1.0",
	scalaVersion := "2.11.8")

lazy val root = (project in file("."))
	.settings(commonSettings: _*)
	.settings(
    name := "scalacommon",
		libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.6",
		  "org.scalaz" %% "scalaz-core" % "7.2.4",
		  "org.scalacheck" %% "scalacheck" % "1.12.1"))
	
