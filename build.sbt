ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.1"

lazy val scala3Version = "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name         := "chess",
    scalaVersion := scala3Version,
    libraryDependencies ++= Dependencies.dependencies,
    Compile / mainClass := Some("chess/Application.scala"),
    fullRunTask(runMigrate, Compile, "chess/DBMigrationsCommand"),
    runMigrate / fork := true,
  )

lazy val runMigrate = taskKey[Unit]("Migrates the database schema.")
addCommandAlias("run-db-migrations", "runMigrate")

