package chess.core

import java.util.UUID
import java.util.HashSet
import cats.effect.Concurrent
import cats.syntax.all.*
import chess.domain.user.Role.*
import chess.domain.user.User
import chess.domain.user.UserRegistration
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.Transactor

trait Users[F[_]] {
  def findByEmail(email: String): F[Option[User]]
  def findByNick(nickname: String): F[Option[User]]
  def findById(id: UUID): F[Option[User]]
  def create(user: User): F[Either[String, UUID]]

  def update(email: Option[String], id: UUID, hashedPassword: String, nickname: String): F[Option[User]]
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

  override def findById(id: UUID): F[Option[User]] =
    sql"SELECT * FROM users WHERE id=$id"
      .query[User]
      .option
      .transact(xa)

  override def create(user: User): F[Either[String, UUID]] =
    findByNick(user.nickname).flatMap {
      case Some(_) => Left("User with this nickname already exists").pure[F]
      case None =>
        user.email match {
          case None =>
            sql"""INSERT INTO users (
                 |elo,
                 |wins,
                 |loses,
                 |allGames,
                 |nickname,
                 |role,
                 |email,
                 |hashedPassword) VALUES (
                 |${user.elo},
                 |${user.wins},
                 |${user.loses},
                 |${user.allGames},
                 |${user.nickname},
                 |${user.role},
                 |${user.email},
                 |${user.hashedPassword})""".stripMargin.update
              .withUniqueGeneratedKeys[UUID]("id")
              .transact(xa)
              .map(Right(_))
          case Some(email) =>
            findByEmail(email).flatMap {
              case None =>
                sql"""INSERT INTO users (
                     |elo,
                     |wins,
                     |loses,
                     |allGames,
                     |nickname,
                     |role,
                     |email,
                     |hashedPassword) VALUES (
                     |${user.elo},
                     |${user.wins},
                     |${user.loses},
                     |${user.allGames},
                     |${user.nickname},
                     |${user.role},
                     |${user.email},
                     |${user.hashedPassword})""".stripMargin.update
                  .withUniqueGeneratedKeys[UUID]("id")
                  .transact(xa)
                  .map(Right(_))
              case Some(_) => Left("This email already exists").pure[F]
            }
        }
    }

  override def update(email: Option[String], id: UUID, hashedPassword: String, nickname: String): F[Option[User]] =
    for {
      _ <-
        sql"""
           UPDATE users SET
           hashedPassword = ${hashedPassword},
           nickname = ${nickname},
           email = ${email}
           WHERE id=${id}""".update.run
          .transact(xa)
      mbUser <- findByNick(nickname)
    } yield mbUser

  override def delete(nickname: String): F[Boolean] =
    sql"DELETE FROM users WHERE nickname=$nickname".update.run.transact(xa).map(_ == 1)
}

object UsersLive {
  def apply[F[_]: Concurrent](xa: Transactor[F]): F[UsersLive[F]] = new UsersLive[F](xa).pure[F]
}
