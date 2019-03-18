package io.github.niblius.erinye.domain.users

import cats._
import cats.data._
import cats.implicits._

class UserService[F[_]: Monad](
    userRepo: UserRepositoryAlgebra[F],
    validation: UserValidationAlgebra[F]) {

  def createUser(user: User): EitherT[F, NonEmptyList[UserValidationError], User] =
    for {
      // not flat map, but apply doesNotExists as validated into the chain
      _ <- EitherT(validation.validate(user).map(_.toEither))
      _ <- validation.doesNotExist(user).leftWiden[UserValidationError]
      saved <- EitherT.right(userRepo.create(user))
    } yield saved

  def getUser(userId: Long): EitherT[F, UserNotFoundError.type, User] =
    EitherT.fromOptionF(userRepo.get(userId), UserNotFoundError)

  def getUserByName(userName: String): EitherT[F, UserNotFoundError.type, User] =
    EitherT.fromOptionF(userRepo.findByUserName(userName), UserNotFoundError)

  def deleteUser(userId: Long): F[Unit] = userRepo.delete(userId).as(())

  def deleteByUserName(userName: String): F[Unit] =
    userRepo.deleteByUserName(userName).as(())

  def update(user: User): EitherT[F, UserValidationError, User] =
    for {
      _ <- validation.exists(user.id.get).leftWiden[UserValidationError]
      _ <- EitherT(validation.validate(user).map(_.toEither))
      saved <- EitherT
        .fromOptionF(userRepo.update(user), UserNotFoundError: UserValidationError)
    } yield saved
}

object UserService {
  def apply[F[_]: Monad](
      repository: UserRepositoryAlgebra[F],
      validation: UserValidationAlgebra[F]): UserService[F] =
    new UserService[F](repository, validation)
}
