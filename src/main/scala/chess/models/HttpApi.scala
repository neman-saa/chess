package chess.models

import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all.*
import chess.http.routes.AuthRoutes
import chess.http.routes.GamesRoute
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.server.Router
import org.http4s.HttpRoutes
import org.typelevel.log4cats.Logger

private class HttpApi[F[_]: Temporal: Logger](core: Core[F], webSocketBuilder: WebSocketBuilder2[F]) {

  private val gamesRoutes = GamesRoute[F](
    webSocketBuilder,
    core.games,
    core.lobbies,
    core.sessions,
    core.users,
    core.auth.authenticator
  ).allRoutes
  private val authRoutes = AuthRoutes[F](core.auth, core.sessions).allRoutes

  val endpoints: HttpRoutes[F] = Router(
    "/chess" -> (gamesRoutes <+> authRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Temporal: Logger](core: Core[F], webSocketBuilder: WebSocketBuilder2[F]): HttpApi[F] =
    new HttpApi[F](core, webSocketBuilder)
}
