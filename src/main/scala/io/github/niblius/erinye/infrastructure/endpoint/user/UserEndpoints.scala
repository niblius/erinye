package io.github.niblius.erinye.infrastructure.endpoint.user

import cats.data.EitherT
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import io.github.niblius.erinye.domain.authentication._
import io.github.niblius.erinye.domain.users.{Role, User, UserService}
import io.github.niblius.erinye.domain._
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import tsec.authentication._
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.PasswordHasher
import tsec.passwordhashers.jca.BCrypt

import scala.language.higherKinds

class UserEndpoints[F[_]: Effect] extends Http4sDsl[F] {
  private def loginEndpoint(
      userService: UserService[F],
      authService: AuthService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "login" =>
        val action = for {
          loginReq <- EitherT.liftF(req.as[LoginRequest])
          resp <- authService.login(loginReq.userName, loginReq.password)
        } yield resp

        action.value.flatMap {
          case Right(token) => Ok(token.asJson)
          case Left(UserAuthenticationFailedError(name)) =>
            BadRequest(s"Authentication failed for user $name")
        }
    }

  private def signupEndpoint(
      userService: UserService[F],
      cryptService: PasswordHasher[F, BCrypt]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "users" =>
        val action = for {
          signup <- req.as[SignupRequest]
          hash <- cryptService.hashpw(signup.password)
          user <- signup.asUser(hash).pure[F]
          result <- userService.createUser(user).value
        } yield result

        action.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(UserAlreadyExistsError(existing)) =>
            Conflict(s"The user with user name ${existing.userName} already exists")
        }
    }

  private def getModifiedUser(
      identity: User,
      crypt: PasswordHasher[F, BCrypt],
      orig: User,
      req: UserUpdateRequest,
  ): EitherT[F, ValidationError, User] = {
    lazy val newHash = req.password
      .map(crypt.hashpw(_).map(_.toString))
      .orElse(orig.hash.map(_.pure[F]))
      .get

    val modified: F[Either[ValidationError, User]] =
      if (identity.role == Role.Administrator)
        for {
          hash <- newHash
          email = req.email.getOrElse(orig.email)
          name = req.name.getOrElse(orig.userName)
          role <- req.role.map(Role(_)).getOrElse(orig.role)
        } yield Either.right(User(name, email, Some(hash), role, orig.id))
      else if (identity.id == orig.id)
        for {
          hash <- newHash
          email = req.email.getOrElse(orig.email)
        } yield Either.right(orig.copy(email = email, hash = Some(hash)))
      else
        Either.left(ForbiddenError).pure

    EitherT(modified)
  }

  private def updateEndpoint(userService: UserService[F], cryptService: PasswordHasher[F, BCrypt])
    : TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AuthenticatedRequired) {
      case secured @ PUT -> Root / "users" / name asAuthed user =>
        val req = secured.request
        val action: EitherT[F, ValidationError, User] = for {
          updateReq <- EitherT.liftF(req.as[UserUpdateRequest])
          origUser <- userService.getUserByName(name)
          updated <- getModifiedUser(user, cryptService, origUser, updateReq)
          result <- userService.update(updated).leftMap[ValidationError](_)
        } yield result

        action.value.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(UserNotFoundError) => NotFound("User not found")
          case Left(ForbiddenError) => Forbidden("You are not allowed to modify this user")
        }
    }

  private def searchByNameEndpoint(userService: UserService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "users" / userName =>
        userService.getUserByName(userName).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(UserNotFoundError) => NotFound("The user was not found")
        }
    }

  private def getUserEndpoint(userService: UserService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" / LongVar(id) =>
        userService.getUser(id).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(UserNotFoundError) => NotFound("The article was not found")
        }
    }

  private def deleteUserEndpoint(
      userService: UserService[F]): TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AdminRequired) {
      case DELETE -> Root / "users" / userName asAuthed _ =>
        for {
          _ <- userService.deleteByUserName(userName)
          resp <- Ok()
        } yield resp
    }

  def endpoints(
      userService: UserService[F],
      cryptService: PasswordHasher[F, BCrypt],
      authService: AuthService[F]): HttpRoutes[F] =
    loginEndpoint(userService, authService) <+>
      signupEndpoint(userService, cryptService) <+>
      searchByNameEndpoint(userService) <+>
      getUserEndpoint(userService) <+>
      authService.Auth.liftService(
        updateEndpoint(userService, cryptService) <+>
          deleteUserEndpoint(userService)
      )
}

object UserEndpoints {
  def endpoints[F[_]: Effect](
      userService: UserService[F],
      cryptService: PasswordHasher[F, BCrypt],
      authService: AuthService[F]): HttpRoutes[F] =
    new UserEndpoints[F].endpoints(userService, cryptService, authService)
}
