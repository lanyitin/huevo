lazy val commonSettings = Seq(
  organization := "tw.lanyitin",
  version := "0.1.0",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "huevo"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6"
