ThisBuild / organization := "tw.lanyitin"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "2.12.8"


lazy val commonSettings = Seq(
  organization := "tw.lanyitin",
  version := "0.1.0",
  // disable using the Scala version in output paths and artifacts
  crossPaths := false
)


lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "huevo",
    isSnapshot := true
  )

libraryDependencies += "tw.lanyitin.common" %% "common-ast" % "0.1.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"


scalacOptions := Seq("-unchecked", "-deprecation", "-feature")
