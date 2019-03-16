package io.github.niblius.erinye.domain.users

import cats.data.EitherT

import scala.language.higherKinds

trait UserValidationAlgebra[F[_]] {

  def doesNotExist(user: User): EitherT[F, UserAlreadyExistsError.type, Unit]

  def exists(userId: Option[Long]): EitherT[F, UserNotFoundError.type, Unit]

  def validateName(username: String): EitherT[F, InvalidUserNameError.type, Unit]

  def validateEmail(email: String): EitherT[F, InvalidEmailError.type, Unit]

  def validateUser(user: User): EitherT[F, UserValidationError, Unit]
}
