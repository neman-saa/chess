package chess.core

import java.util.UUID

import scala.concurrent.duration.*

import cats.data.OptionT
import cats.effect.kernel.Async
import cats.effect.Ref
import cats.syntax.all.*
import chess.configuration.SecurityConfig
import chess.core.Users
import chess.domain.auth.NewPasswordInfo
import chess.domain.security.*
import chess.domain.user.*
import org.typelevel.log4cats.Logger
import tsec.authentication.BackingStore
import tsec.authentication.IdentityStore
import tsec.authentication.JWTAuthenticator
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

trait Auth[F[_]] {
  def login(nickname: String, password: String): F[Option[JwtToken]]
  def signUp(userRegistration: UserRegistration): F[Option[User]]
  def changePassword(nickname: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]]
  def changeNickname(oldNick: String, newNick: String): F[Either[String, Option[User]]]
  def authenticator: Authenticator[F]
  def delete(nickname: String): F[Boolean]
}
class LiveAuth[F[_]: Async: Logger](users: Users[F], authenticatorr: Authenticator[F], sessions: Sessions[F])
    extends Auth[F] {
  override def login(nickname: String, password: String): F[Option[JwtToken]] = for {
    maybeUser <- users.findByNick(nickname)
    mbValidateUser <- maybeUser.filterA(user =>
      BCrypt.checkpwBool[F](password, PasswordHash[BCrypt](user.hashedPassword))
    )
    mbJwtToken <- mbValidateUser.traverse(user =>
      sessions.add(user.id).flatMap(_ => authenticatorr.create(user.nickname))
    )
  } yield mbJwtToken

  override def signUp(userRegistration: UserRegistration): F[Option[User]] = {
    val signUpF = for {
      hashedPassword <- BCrypt.hashpw[F](userRegistration.pass)
      creation <- users.create(
        User(
          UUID.randomUUID(),
          1000,
          0,
          0,
          0,
          userRegistration.nickname,
          Role.PLAYER,
          userRegistration.mbEmail,
          hashedPassword
        )
      )
    } yield creation

    signUpF.flatMap {
      case Left(_)       => None.pure[F]
      case Right(userId) => users.findById(userId)
    }
  }

  override def changePassword(nickname: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]] =
    users.findByNick(nickname).flatMap {
      case None => Left("No such user").pure[F]
      case Some(user) =>
        for {
          isCorrectOld <- BCrypt.checkpwBool[F](newPasswordInfo.oldPassword, PasswordHash[BCrypt](user.hashedPassword))
          result <-
            if (isCorrectOld) for {
              hashedPassword <- BCrypt.hashpw[F](newPasswordInfo.newPassword)
              updatedUser    <- users.update(user.email, user.id, hashedPassword, user.nickname)
            } yield Right(updatedUser)
            else Left("Incorrect password").pure[F]
        } yield result
    }

  override def changeNickname(oldNick: String, newNick: String): F[Either[String, Option[User]]] =
    users.findByNick(oldNick).flatMap {
      case None       => Left("No such user").pure[F]
      case Some(user) => users.update(user.email, user.id, user.hashedPassword, user.nickname).map(Right(_))
    }

  override def authenticator: Authenticator[F] = authenticatorr

  override def delete(nickname: String): F[Boolean] = users.delete(nickname)
}

object LiveAuth {
  def apply[F[_]: Async: Logger](
      users: Users[F],
      securityConfig: SecurityConfig,
      sessions: Sessions[F]
  ): F[LiveAuth[F]] = {
    val keyF                                    = HMACSHA256.buildKey[F](securityConfig.secret.getBytes("UTF-8"))
    val idStore: IdentityStore[F, String, User] = (nickname: String) => OptionT(users.findByNick(nickname))
    val tokenStoreF = Ref.of[F, Map[SecureRandomId, JwtToken]](Map.empty).map { ref =>
      new BackingStore[F, SecureRandomId, JwtToken] {
        override def get(id: SecureRandomId): OptionT[F, JwtToken] = OptionT(ref.get.map(_.get(id)))
        override def put(elem: JwtToken): F[JwtToken]              = ref.modify(store => (store + (elem.id -> elem), elem))
        override def update(v: JwtToken): F[JwtToken]              = put(v)
        override def delete(id: SecureRandomId): F[Unit]           = ref.modify(store => (store - id, ()))
      }
    }
    val authenticatorF: F[Authenticator[F]] = for {
      key        <- keyF
      tokenStore <- tokenStoreF
    } yield JWTAuthenticator.backed.inBearerToken(
      expiryDuration = securityConfig.jwtExpiryDuration,
      maxIdle = None,
      identityStore = idStore,
      tokenStore = tokenStore,
      signingKey = key
    )

    authenticatorF.map(authenticator => new LiveAuth[F](users, authenticator, sessions))
  }
}
