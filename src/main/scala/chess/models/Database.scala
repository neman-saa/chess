package chess.models

import cats.effect.Async
import cats.effect.Resource
import chess.configuration.PostgresConfig
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts

object Database {

  def postgresResource[F[_]: Async](config: PostgresConfig): Resource[F, HikariTransactor[F]] = for {
    ec <- ExecutionContexts.fixedThreadPool(config.nThreads)
    xa <- HikariTransactor.newHikariTransactor[F](
      "org.postgresql.Driver",
      config.url,
      config.username,
      config.password,
      ec
    )
  } yield xa
}
