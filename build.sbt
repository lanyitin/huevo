// enablePlugins(ScalaJSPlugin)

ThisBuild / organization := "tw.lanyitin"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "2.12.8"


lazy val commonSettings = Seq(
  organization := "tw.lanyitin",
  version := "0.1.0",
)

lazy val commonAst = RootProject(file("../common-ast"))


lazy val root = (project in file(".")).
  aggregate(commonAst).
  dependsOn(commonAst).
  settings(commonSettings: _*).
  settings(
    name := "huevo",
    isSnapshot := true
  )

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")
