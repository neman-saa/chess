package chess.models

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.syntax.all.*
import chess.configuration.SecurityConfig
import chess.core._
import chess.core.UsersLive
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger
final class Core[F[_]](
    val auth: Auth[F],
    val games: Games[F],
    val lobbies: Lobby[F],
    val sessions: Sessions[F],
    val users: Users[F]
)
object Core {
  def apply[F[_]: Async: Logger](xa: Transactor[F], securityConfig: SecurityConfig): Resource[F, Core[F]] = {
    val coreF = for {
      users    <- UsersLive[F](xa)
      sessions <- SessionsLive[F]
      auth     <- LiveAuth[F](users, securityConfig, sessions)
      games    <- LiveGames[F](xa)(sessions, users)
      lobbies  <- LobbyLive[F](games, sessions)
    } yield new Core(auth, games, lobbies, sessions, users)

    Resource.eval(coreF)
  }
}
