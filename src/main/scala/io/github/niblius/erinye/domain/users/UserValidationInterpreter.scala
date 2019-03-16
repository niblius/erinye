package io.github.niblius.erinye.domain.users

import cats._
import cats.data.EitherT
import cats.implicits._

class UserValidationInterpreter[F[_]: Monad](userRepo: UserRepositoryAlgebra[F])
    extends UserValidationAlgebra[F] {
  override def doesNotExist(user: User) = EitherT {
    userRepo.findByUserName(user.userName).map {
      case None => Right(())
      case Some(_) => Left(UserAlreadyExistsError)
    }
  }

  override def exists(userId: Option[Long]): EitherT[F, UserNotFoundError.type, Unit] =
    EitherT {
      userId
        .map { id =>
          userRepo.get(id).map {
            case Some(_) => Right(())
            case _ => Left(UserNotFoundError)
          }
        }
        .getOrElse(
          Either.left[UserNotFoundError.type, Unit](UserNotFoundError).pure[F]
        )
    }

  private val usernameRegex = """^(?=.{6,20}$)(?![_.])(?!.*[_.]{2})[a-zA-Z0-9._]+(?<![_.])$""".r
  override def validateName(username: String): EitherT[F, InvalidUserNameError.type, Unit] =
    username match {
      case usernameRegex(_) => EitherT.rightT(())
      case _ => EitherT.leftT(InvalidUserNameError)
    }

  // omg, right?
  // https://stackoverflow.com/questions/201323/how-to-validate-an-email-address-using-a-regular-expression
  private val emailRegex =
    """(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])""".r
  override def validateEmail(email: String): EitherT[F, InvalidEmailError.type, Unit] =
    email match {
      case emailRegex(_) => EitherT.rightT(())
      case _ => EitherT.leftT(InvalidEmailError)
    }

  // TODO: Use cats' form validation
  override def validateUser(user: User): EitherT[F, UserValidationError, Unit] =
    for {
      _ <- validateEmail(user.email).leftWiden[UserValidationError]
      _ <- validateName(user.userName).leftWiden[UserValidationError]
    } yield ()

}

object UserValidationInterpreter {
  def apply[F[_]: Monad](repo: UserRepositoryAlgebra[F]): UserValidationAlgebra[F] =
    new UserValidationInterpreter[F](repo)
}
