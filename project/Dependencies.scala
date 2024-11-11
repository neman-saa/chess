import sbt.*
object Dependencies {
  lazy val circeVersion               = "0.14.0"
  lazy val catsEffectVersion          = "3.3.14"
  lazy val http4sVersion              = "0.23.15"
  lazy val doobieVersion              = "1.0.0-RC1"
  lazy val pureConfigVersion          = "0.17.1"
  lazy val log4catsVersion            = "2.4.0"
  lazy val tsecVersion                = "0.4.0"
  lazy val scalaTestVersion           = "3.2.12"
  lazy val scalaTestCatsEffectVersion = "1.4.0"
  lazy val testContainerVersion       = "1.17.3"
  lazy val logbackVersion             = "1.4.0"
  lazy val slf4jVersion               = "2.0.0"
  lazy val flywayVersion              = "9.16.0"

  val dependencies: Seq[ModuleID] = Seq(
    "org.typelevel"         %% "cats-effect"                   % catsEffectVersion,
    "org.http4s"            %% "http4s-dsl"                    % http4sVersion,
    "org.http4s"            %% "http4s-ember-server"           % http4sVersion,
    "org.http4s"            %% "http4s-ember-client"           % http4sVersion,
    "org.http4s"            %% "http4s-circe"                  % http4sVersion,
    "io.circe"              %% "circe-generic"                 % circeVersion,
    "io.circe"              %% "circe-fs2"                     % circeVersion,
    "org.tpolecat"          %% "doobie-core"                   % doobieVersion,
    "org.tpolecat"          %% "doobie-hikari"                 % doobieVersion,
    "org.tpolecat"          %% "doobie-postgres"               % doobieVersion,
    "org.tpolecat"          %% "doobie-scalatest"              % doobieVersion              % Test,
    "com.github.pureconfig" %% "pureconfig-core"               % pureConfigVersion,
    "org.typelevel"         %% "log4cats-slf4j"                % log4catsVersion,
    "org.slf4j"              % "slf4j-simple"                  % slf4jVersion,
    "io.github.jmcardon"    %% "tsec-http4s"                   % tsecVersion,
    "org.flywaydb"           % "flyway-core"                   % flywayVersion,
    "org.typelevel"         %% "log4cats-noop"                 % log4catsVersion            % Test,
    "org.scalatest"         %% "scalatest"                     % scalaTestVersion           % Test,
    "org.typelevel"         %% "cats-effect-testing-scalatest" % scalaTestCatsEffectVersion % Test,
    "org.testcontainers"     % "testcontainers"                % testContainerVersion       % Test,
    "org.testcontainers"     % "postgresql"                    % testContainerVersion       % Test,
    "ch.qos.logback"         % "logback-classic"               % logbackVersion             % Test
  )
}