package chess.domain
import cats.*
import cats.implicits.*
import chess.domain.user.Role
import chess.domain.user.User
import org.http4s.Response
import org.http4s.Status
import tsec.authentication.AugmentedJWT
import tsec.authentication.JWTAuthenticator
import tsec.authentication.SecuredRequest
import tsec.authentication.TSecAuthService
import tsec.authorization.AuthorizationInfo
import tsec.authorization.BasicRBAC // if use ._ does not work
import tsec.mac.jca.HMACSHA256

object security {
  type JwtToken               = AugmentedJWT[HMACSHA256, String]
  type Authenticator[F[_]]    = JWTAuthenticator[F, String, User, HMACSHA256]
  type AuthRoutes[F[_]]       = PartialFunction[SecuredRequest[F, User, JwtToken], F[Response[F]]]
  private type AuthRBAC[F[_]] = BasicRBAC[F, Role, User, JwtToken]

  given authRole[F[_]: MonadThrow]: AuthorizationInfo[F, Role, User] with {
    override def fetchInfo(u: User): F[Role] = u.role.pure[F]
  }

  def allRoles[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC.all[F, Role, User, JwtToken]

  def adminOnly[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC(Role.ADMIN)

  def playersOnly[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC(Role.PLAYER)

  case class Authorizations[F[_]](rbacRoutes: Map[AuthRBAC[F], List[AuthRoutes[F]]])

  private object Authorizations {
    given combiner[F[_]]: Semigroup[Authorizations[F]] =
      Semigroup.instance((authA, authB) => Authorizations(authA.rbacRoutes |+| authB.rbacRoutes))
  }

  extension [F[_]](authRoutes: AuthRoutes[F])
    def restrictedTo(rbac: AuthRBAC[F]): Authorizations[F] =
      Authorizations(Map(rbac -> List(authRoutes)))

  given authToSec[F[_]: Monad]: Conversion[Authorizations[F], TSecAuthService[User, JwtToken, F]] =
    auths => {
      val unAuthorizedService =
        TSecAuthService[User, JwtToken, F] { _ =>
          Response[F](Status.Unauthorized).pure[F]
        }
      auths.rbacRoutes.toSeq.foldLeft(unAuthorizedService) {
        case (acc, (rbac, routes)) =>
          val bigRoute = routes.reduce(_ orElse _)
          TSecAuthService.withAuthorizationHandler(rbac)(bigRoute, acc.run)
      }
    }
}
