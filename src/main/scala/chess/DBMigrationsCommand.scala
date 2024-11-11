package chess

import cats.effect.syntax.all.*
import cats.effect.IO
import cats.effect.IOApp
import cats.instances.list.*
import cats.syntax.all.*
import chess.configuration.syntax.*
import chess.configuration.AppConfig
import chess.configuration.JdbcDatabaseConfig
import chess.models.DBMigrations
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import pureconfig.ConfigSource
object DBMigrationsCommand extends IOApp.Simple {

  private val dbConfigNamespaces: List[String] = List("chess.jdbc")

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  override def run: IO[Unit] = dbConfigNamespaces.traverse { namespace =>
    for {
      _   <- Logger[IO].info(s"Migrating database configuration: $namespace")
      cfg <- ConfigSource.default.loadF[IO, AppConfig].map(_.jdbcDatabaseConfig)
      _   <- DBMigrations.migrate[IO](cfg)
    } yield ()
  }.void
}
