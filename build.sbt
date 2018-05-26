lazy val commonSettings = Seq(
  organization := "tw.lanyitin",
  version := "0.1.0",
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "huevo"
  )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

scalaVersion := "2.12.6"