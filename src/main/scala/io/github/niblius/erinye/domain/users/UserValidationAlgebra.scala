package io.github.niblius.erinye.domain.users

import cats.data.EitherT
import io.github.niblius.erinye.domain.{UserAlreadyExistsError, UserNotFoundError}

import scala.language.higherKinds

trait UserValidationAlgebra[F[_]] {

  def doesNotExist(user: User): EitherT[F, UserAlreadyExistsError, Unit]

  def exists(userId: Option[Long]): EitherT[F, UserNotFoundError.type, Unit]
}
