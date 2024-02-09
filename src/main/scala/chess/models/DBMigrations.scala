package chess.models

import scala.jdk.CollectionConverters._

import cats.effect.syntax.all.*
import cats.effect.Sync
import cats.implicits._
import chess.configuration.JdbcDatabaseConfig
import org.flywaydb.core.api.configuration.FluentConfiguration
import org.flywaydb.core.api.Location
import org.flywaydb.core.internal.command.DbValidate
import org.flywaydb.core.Flyway
import org.typelevel.log4cats.Logger
object DBMigrations {

  def migrate[F[_]: Sync: Logger](config: JdbcDatabaseConfig): F[Int] =
    Logger[F].info(
      "Running migrations from locations: " +
        config.migrationsLocations.mkString(", ")
    ) >> {
      val count = unsafeMigrate(config)
      Logger[F].info(s"Executed $count migrations") >> count.pure[F]
    }

  private def unsafeMigrate(config: JdbcDatabaseConfig): Int = {
    val m: FluentConfiguration = Flyway.configure
      .dataSource(
        config.url,
        config.user,
        config.password
      )
      .group(true)
      .outOfOrder(false)
      .table(config.migrationsTable)
      .locations(
        config.migrationsLocations
          .map(new Location(_)): _*
      )
      .baselineOnMigrate(true)

    m.load().migrate().migrationsExecuted
  }

}
