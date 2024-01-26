package chess.core

import cats.effect.Concurrent
import chess.domain.user.{User, UserRegistration}
import doobie.Transactor
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.*
import cats.syntax.all.*
import chess.domain.user.Role.*
import java.util.UUID

trait Users[F[_]] {
  def findByEmail(email: String): F[Option[User]]
  def findByNick(nickname: String): F[Option[User]]
  def create(user: User): F[Either[String, UUID]]

  def update(email: String, id: UUID, hashedPassword: String, nickname: String): F[Option[User]]
  def delete(email: String): F[Boolean]

}

class UsersLive[F[_]: Concurrent](xa: Transactor[F]) extends Users[F] {
  override def findByEmail(email: String): F[Option[User]] =
    sql"SELECT * FROM users WHERE email=$email"
      .query[User]
      .option
      .transact(xa)

  override def findByNick(nickname: String): F[Option[User]] =
    sql"SELECT * FROM users WHERE nickname=$nickname"
      .query[User]
      .option
      .transact(xa)

  override def create(user: User): F[Either[String, UUID]] =
    findByEmail(user.email).flatMap {
      case Some(_) => Left("User with this email already exists").pure[F]
      case None => findByNick(user.nickname).flatMap {
        case None =>
          sql"""INSERT INTO users (
          |elo,
          |wins,
          |loses,
          |allGames,
          |nickname,
          |role,
          |email,
          |hashedPassword) VALUE (
          |${user.elo},
          |${user.wins},
          |${user.loses},
          |${user.allGames},
          |${user.nickname},
          |${user.role},
          |${user.email},
          |${user.hashedPassword});"""
          .stripMargin
          .update
          .withUniqueGeneratedKeys[UUID]("id")
          .transact(xa).map(Right(_))
        case Some(_) => Left("This nickname already exists").pure[F]
      }
    }

  override def update(email: String, id: UUID, hashedPassword: String, nickname: String): F[Option[User]] = for {
    _ <-
      sql"""
           UPDATE users SET
           hashedPassword = ${hashedPassword},
           nickname = ${nickname},
           email = ${email}
           WHERE id=${id}"""
           .update
           .run
           .transact(xa)
    mbUser <- findByEmail(email)
  } yield mbUser

  override def delete(email: String): F[Boolean] =
    sql"DELETE FROM users WHERE email=$email".update.run.transact(xa).map(_ == 1)
}

object UsersLive {
  def apply[F[_]: Concurrent](xa: Transactor[F]): F[UsersLive[F]] = new UsersLive[F](xa).pure[F]
}
