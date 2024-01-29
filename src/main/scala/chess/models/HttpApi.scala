package chess.models

import cats.effect.{Concurrent, Resource}
import chess.http.routes.{AuthRoutes, GamesRoute}
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import cats.syntax.all.*

private class HttpApi[F[_]: Concurrent: Logger](core: Core[F], webSocketBuilder: WebSocketBuilder2[F]) {

  private val gamesRoutes = GamesRoute[F](webSocketBuilder, core.games, core.lobbies, core.sessions, core.users, core.auth.authenticator).allRoutes
  private val authRoutes = AuthRoutes[F](core.auth, core.sessions).allRoutes
  
  val endpoints: HttpRoutes[F] = Router(
    "/chess" -> (gamesRoutes <+> authRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent: Logger](core: Core[F], webSocketBuilder: WebSocketBuilder2[F]): HttpApi[F] = 
   new HttpApi[F](core, webSocketBuilder)
}
