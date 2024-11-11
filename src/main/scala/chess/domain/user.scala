package chess.domain

import java.util.UUID

import doobie.Meta
import tsec.authorization.AuthGroup
import tsec.authorization.SimpleAuthEnum

object user {

  case class User(
      id: UUID,
      elo: Int,
      wins: Int,
      loses: Int,
      allGames: Int,
      nickname: String,
      role: Role,
      email: Option[String],
      hashedPassword: String
  )

  enum Role {
    case ADMIN, PLAYER
  }

  object Role {
    given metaRole: Meta[Role] = Meta[String].timap[Role](Role.valueOf)(_.toString)
  }

  given roleAuthEnum: SimpleAuthEnum[Role, String] with {
    override val values: AuthGroup[Role] = AuthGroup(Role.ADMIN, Role.PLAYER)

    override def getRepr(role: Role): String = role.toString
  }

  case class UserRegistration(pass: String, nickname: String, mbEmail: Option[String] = None)
}
