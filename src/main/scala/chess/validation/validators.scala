package chess.validation

import java.net.URL

import scala.util.Success
import scala.util.Try

import cats.data.ValidatedNel
import cats.syntax.all.*
import chess.domain.auth.LoginInfo
import chess.domain.auth.NewPasswordInfo
import chess.domain.user.UserRegistration
object validators {

  trait ValidationFailure(val message: String)

  private case class EmptyField(fieldName: String)   extends ValidationFailure(s"field $fieldName is empty")
  private case class InvalidUrl(fieldName: String)   extends ValidationFailure(s"$fieldName is not a valid URL")
  private case class InvalidEmail(fieldName: String) extends ValidationFailure(s"$fieldName is not a valid email")
  case class InvalidPassword(fieldName: String)
      extends ValidationFailure(s"password $fieldName is not a valid password")

  trait Validator[A] {
    def validate(a: A): ValidatedNel[ValidationFailure, A]
  }

  private def validateRequired[A](field: A, fieldName: String)(
      required: A => Boolean
  ): ValidatedNel[ValidationFailure, A] =
    if (required(field)) field.validNel
    else EmptyField(fieldName).invalidNel

  private def validateUrl(field: String, fieldName: String): ValidatedNel[ValidationFailure, String] =
    Try(URL(field).toURI) match {
      case Success(_) => field.validNel
      case _          => InvalidUrl(field).invalidNel
    }

  private def validateEmail(field: String, fieldName: String): ValidatedNel[ValidationFailure, String] = {
    val emailRegex =
      """^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    if (emailRegex.findFirstMatchIn(field).isDefined) field.validNel
    else InvalidEmail(fieldName).invalidNel
  }

  given userRegistrationValidator: Validator[UserRegistration] = (userRegistration: UserRegistration) => {
    val validNickName = validateRequired(userRegistration.nickname, "username")(nick =>
      !nick.contains('-') && nick.length > 4 && nick.length < 15
    )
    val validatePass = validateRequired(userRegistration.pass, "password")(pass => pass.length > 5 && pass.length < 30)
    val validateEmaill = userRegistration.mbEmail match {
      case None        => None.validNel
      case Some(email) => validateEmail(email, "email").map(Some(_))
    }

    (validatePass, validNickName, validateEmaill).mapN(UserRegistration.apply)
  }

  given newPasswordValidator: Validator[NewPasswordInfo] = (newPasswordInfo: NewPasswordInfo) => {
    val validatePassword =
      validateRequired(newPasswordInfo.newPassword, "new password")(pass => pass.length > 5 && pass.length < 30)
    validatePassword.map(NewPasswordInfo(newPasswordInfo.oldPassword, _))
  }

}
