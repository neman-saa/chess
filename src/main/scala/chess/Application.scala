package chess

import cats.effect.kernel.Resource
import cats.effect.IO
import cats.effect.IOApp
import chess.configuration.syntax.*
import chess.configuration.AppConfig
import chess.models.*
import chess.models.Database
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import doobie.util.transactor.Transactor
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import pureconfig.ConfigSource
object Application extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] = ConfigSource.default.loadF[IO, AppConfig].flatMap {
    case AppConfig(emberConfig, postgresConfig, securityConfig, _) =>
      val appResource = for {
        xa: Transactor[IO] <- Database.postgresResource[IO](postgresConfig)
        core               <- Core[IO](xa, securityConfig)
        server <- EmberServerBuilder
          .default[IO]
          .withHost(emberConfig.host)
          .withPort(emberConfig.port)
          .withHttpWebSocketApp(webSocketBuilder => HttpApi[IO](core, webSocketBuilder).endpoints.orNotFound)
          .build
      } yield server

      appResource.use(_ => IO.println("Server started") *> IO.never)
  }
}
