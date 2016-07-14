lazy val commonSettings = Seq(
	organization := "org.me",
	version := "1.0",
	scalaVersion := "2.11.8")

lazy val root = (project in file("."))
	.settings(commonSettings: _*)
	.settings(
    name := "ScalaCommon",
		libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
		  "org.scalaz" % "scalaz-core_2.11" % "7.2.4",
		  "org.scalacheck" % "scalacheck_2.11" % "1.12.1" % "test"))
	
