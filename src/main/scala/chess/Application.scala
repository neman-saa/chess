package chess

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Resource
import chess.configuration.AppConfig
import chess.models.Database
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import chess.models.*
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import org.http4s.ember.server.EmberServerBuilder
import pureconfig.ConfigSource
import chess.configuration.syntax.*
object Application extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] = ConfigSource.default.loadF[IO, AppConfig].flatMap {
    case AppConfig(emberConfig, postgresConfig, securityConfig) =>
    val appResource = for {
      xa: Transactor[IO] <- Database.postgresResource[IO](postgresConfig)
      core <- Core[IO](xa, securityConfig)
      server <- EmberServerBuilder
        .default[IO]
        .withHost(emberConfig.host)
        .withPort(emberConfig.port)
        .withHttpWebSocketApp( webSocketBuilder =>
          HttpApi[IO](core, webSocketBuilder).endpoints.orNotFound
        ).build
    } yield server

    appResource.use(_ => IO.println("Server started") *> IO.never)
  }
}
