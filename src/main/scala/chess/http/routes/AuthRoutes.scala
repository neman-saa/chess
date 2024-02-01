package chess.http.routes

import cats.effect.Concurrent
import cats.syntax.all.*
import chess.core.Auth
import chess.core.Sessions
import chess.domain.auth.LoginInfo
import chess.domain.auth.NewPasswordInfo
import chess.domain.security.*
import chess.domain.user.*
import chess.validation.syntax.HttpValidationDsl
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.RequestDslBinCompat
import org.http4s.server.Router
import org.http4s.EntityDecoder
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.Status
import org.typelevel.log4cats.Logger
import tsec.authentication.asAuthed
import tsec.authentication.SecuredRequestHandler
import tsec.authentication.TSecAuthService

class AuthRoutess[F[_]: Concurrent: Logger](auth: Auth[F], sessions: Sessions[F]) extends HttpValidationDsl[F] {

  val dsl: Http4sDsl[F] with RequestDslBinCompat = Http4sDsl[F]
  import dsl.*

  private val authenticator = auth.authenticator
  private val securedHandler: SecuredRequestHandler[F, String, User, JwtToken] =
    SecuredRequestHandler(authenticator)

  private val loginRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "login" =>
      for {
        loginInfo <- req.as[LoginInfo]
        isLogged  <- auth.login(loginInfo.nickname, loginInfo.password)
      } yield isLogged match {
        case None        => Response(Status.Unauthorized)
        case Some(token) => authenticator.embed(Response(Status.Ok), token)
      }
  }

  private val signUp: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "users" / "signUp" =>
      req.validate[UserRegistration] { userRegistration =>
        for {
          mbNewUser <- auth.signUp(userRegistration)
          response <- mbNewUser match {
            case Some(user) => Created(user.nickname)
            case None       => BadRequest("User with this email already exists")
          }
        } yield response
      }
  }

  private val resetPasswordRoute: AuthRoutes[F] = {
    case req @ POST -> Root / "users" / "password" asAuthed user =>
      req.request.validate[NewPasswordInfo](newPasswordInfo =>
        auth.changePassword(user.nickname, newPasswordInfo).flatMap {
          case Left(e)        => Forbidden()
          case Right(Some(_)) => Ok()
          case Right(None)    => NotFound("User with this email not found, this should not be")
        }
      )
  }

  private val deleteUser: AuthRoutes[F] = {
    case req @ POST -> Root / "users" / "delete" asAuthed user =>
      auth.delete(user.nickname).flatMap {
        case true  => Ok()
        case false => NotFound()
      }
  }

  private val logoutRoute: AuthRoutes[F] = {
    case req @ POST -> Root / "users" / "logout" asAuthed user =>
      val token = req.authenticator
      for {
        _    <- authenticator.discard(token)
        _    <- sessions.update(user.id, None)
        resp <- Ok()
      } yield resp
  }

  private val unAuthedRoutes = loginRoute <+> signUp
  private val authedRoutes = securedHandler.liftService(
    logoutRoute.restrictedTo(allRoles) |+|
      deleteUser.restrictedTo(adminOnly) |+|
      resetPasswordRoute.restrictedTo(allRoles)
  )

  val allRoutes: HttpRoutes[F] = Router(
    "/auth" -> (unAuthedRoutes <+> authedRoutes)
  )
}

object AuthRoutes {
  def apply[F[_]: Concurrent: Logger](auth: Auth[F], sessions: Sessions[F]): AuthRoutess[F] =
    new AuthRoutess[F](auth, sessions)
}
