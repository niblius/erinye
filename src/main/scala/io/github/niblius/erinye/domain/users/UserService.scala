package io.github.niblius.erinye.domain.users

import cats._
import cats.data._
import cats.implicits._

class UserService[F[_]: Monad](
    userRepo: UserRepositoryAlgebra[F],
    validation: UserValidationAlgebra[F]) {

  private def createUserWithExternalValidation(
      user: User,
      validate: F[ValidatedNel[UserValidationError, Unit]])
    : EitherT[F, NonEmptyList[UserValidationError], User] =
    for {
      _ <- EitherT(validate.map(_.toEither))
      saved <- EitherT.right(userRepo.create(user))
    } yield saved

  def createUser(user: User): EitherT[F, NonEmptyList[UserValidationError], User] = {
    val validate = for {
      fields <- validation.validate(user)
      uniqueness <- validation.doesNotExist(user).toValidatedNel
    } yield fields |+| uniqueness

    createUserWithExternalValidation(user, validate)
  }

  /**
    * We need not hashed version of a password to validate it.
    */
  def createUserCheckingPassword(
      user: User,
      notHashedPassword: String): EitherT[F, NonEmptyList[UserValidationError], User] = {
    val validate = for {
      fields <- validation.validate(user)
      password <- validation
        .validatePassword(notHashedPassword)
        .toValidatedNel
      uniqueness <- validation.doesNotExist(user).toValidatedNel
    } yield fields |+| password |+| uniqueness
    createUserWithExternalValidation(user, validate)
  }

  def getUser(userId: Long): EitherT[F, UserNotFoundError.type, User] =
    EitherT.fromOptionF(userRepo.get(userId), UserNotFoundError)

  def getUserByName(userName: String): EitherT[F, UserNotFoundError.type, User] =
    EitherT.fromOptionF(userRepo.findByUserName(userName), UserNotFoundError)

  def deleteUser(userId: Long): F[Unit] = userRepo.delete(userId).as(())

  def deleteByUserName(userName: String): F[Unit] =
    userRepo.deleteByUserName(userName).as(())

  private def updateWithExternalValidation(
      user: User,
      validate: F[ValidatedNel[UserValidationError, Unit]])
    : EitherT[F, NonEmptyList[UserValidationError], User] =
    for {
      _ <- EitherT(validate.map(_.toEither))
      saved <- EitherT
        .fromOptionF(
          userRepo.update(user),
          NonEmptyList.one(UserNotFoundError: UserValidationError))
    } yield saved

  /**
    * We need not hashed version of a password to validate it.
    */
  def updateCheckingPassword(
      user: User,
      notHashedPassword: String): EitherT[F, NonEmptyList[UserValidationError], User] = {
    val validate = for {
      fields <- validation.validate(user)
      password <- validation
        .validatePassword(notHashedPassword)
        .toValidatedNel
      existing <- validation.exists(user.id.get).toValidatedNel
    } yield fields |+| password |+| existing
    updateWithExternalValidation(user, validate)
  }

  def update(user: User): EitherT[F, NonEmptyList[UserValidationError], User] = {
    val validate = for {
      fields <- validation.validate(user)
      existing <- validation.exists(user.id.get).toValidatedNel
    } yield fields.combine(existing)

    updateWithExternalValidation(user, validate)
  }
}

object UserService {
  def apply[F[_]: Monad](
      repository: UserRepositoryAlgebra[F],
      validation: UserValidationAlgebra[F]): UserService[F] =
    new UserService[F](repository, validation)
}
