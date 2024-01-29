package chess.domain

object auth {
  final case class LoginInfo(nickname: String, password: String)
  final case class NewPasswordInfo(oldPassword: String, newPassword: String)
}
