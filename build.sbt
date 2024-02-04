ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.1"

lazy val scala3Version = "3.2.1"

lazy val server = (project in file("."))
  .settings(
    name         := "root",
    scalaVersion := scala3Version,
    libraryDependencies ++= Dependencies.dependencies,
    Compile / mainClass := Some("chess/Application.scala")
  )
