// enablePlugins(ScalaJSPlugin)

lazy val commonSettings = Seq(
  organization := "tw.lanyitin",
  version := "0.1.0",
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "huevo",
    scalaJSUseMainModuleInitializer := true,
    scalaVersion := "2.12.7"
  )

isSnapshot := true

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")
