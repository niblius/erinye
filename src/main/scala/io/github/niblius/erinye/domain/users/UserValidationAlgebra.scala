package io.github.niblius.erinye.domain.users

import cats.data.{EitherT, ValidatedNel}

import scala.language.higherKinds

trait UserValidationAlgebra[F[_]] {

  def doesNotExist(user: User): EitherT[F, UserValidationError, Unit]

  def exists(userId: Long): EitherT[F, UserNotFoundError.type, Unit]

  def validatePassword(password: String): EitherT[F, InvalidPasswordError.type, Unit]

  def validateName(username: String): EitherT[F, InvalidUserNameError.type, Unit]

  def validateEmail(email: String): EitherT[F, InvalidEmailError.type, Unit]

  def validate(user: User): F[ValidatedNel[UserValidationError, Unit]]
}
