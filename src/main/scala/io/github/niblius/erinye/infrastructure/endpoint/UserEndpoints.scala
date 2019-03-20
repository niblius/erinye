package io.github.niblius.erinye.infrastructure.endpoint

import cats.Applicative
import cats.data.{EitherT, NonEmptyList}
import cats.effect.{Effect, Sync}
import cats.implicits._
import io.circe.generic.auto._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.jsonOf
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import tsec.authentication._
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.{PasswordHash, PasswordHasher}
import tsec.passwordhashers.jca.BCrypt
import io.github.niblius.erinye.domain.authentication._
import io.github.niblius.erinye.domain.users._

import scala.language.higherKinds

final case class LoginRequest(userName: String, password: String)
object LoginRequest {
  implicit def loginRequestEnc[F[_]: Applicative]: EntityEncoder[F, LoginRequest] =
    jsonEncoderOf
  implicit def loginReqDec[F[_]: Sync]: EntityDecoder[F, LoginRequest] = jsonOf
}

final case class SignupRequest(userName: String, email: String, password: String) {
  def asUser[A](hashedPassword: PasswordHash[A]): User = User(
    userName,
    email,
    Some(hashedPassword.toString)
  )
}
object SignupRequest {
  implicit def signupRequestEnc[F[_]: Applicative]: EntityEncoder[F, SignupRequest] = jsonEncoderOf
  implicit def signupReqDec[F[_]: Sync]: EntityDecoder[F, SignupRequest] = jsonOf
}

final case class UserUpdateRequest(
    name: Option[String],
    email: Option[String],
    password: Option[String],
    role: Option[String]
)
object UserUpdateRequest {
  implicit def userUpdateRequestEnc[F[_]: Applicative]: EntityEncoder[F, UserUpdateRequest] =
    jsonEncoderOf
  implicit def userUpdateRequestDec[F[_]: Sync]: EntityDecoder[F, UserUpdateRequest] = jsonOf
}

class UserEndpoints[F[_]: Effect] extends Http4sDsl[F] {
  private val responseBuilder = ResponseBuilder[F, UserValidationError](
    {
      case UserNotFoundError => NotFound(_)
      case UserAlreadyExistsError =>
        Conflict(_)
      case UserForbiddenError => Forbidden(_)
      case _ => BadRequest(_)
    },
    jsons => BadRequest(jsons),
    json => Ok(json)
  )

  private def loginEndpoint(authService: AuthService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "login" =>
        val action: EitherT[F, UserValidationError, Token] = for {
          loginReq <- EitherT.liftF(req.as[LoginRequest])
          resp <- authService
            .login(loginReq.userName, loginReq.password)
            .leftWiden[UserValidationError]
        } yield resp

        responseBuilder.build(action.value)
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
          result <- userService.createUserCheckingPassword(user, signup.password).value
        } yield result

        responseBuilder.buildList(action)
    }

  private def getModifiedUser(
      identity: User,
      crypt: PasswordHasher[F, BCrypt],
      orig: User,
      req: UserUpdateRequest,
  ): EitherT[F, UserValidationError, User] = {
    lazy val newHash = req.password
      .map(crypt.hashpw(_).map(_.toString))
      .orElse(orig.hash.map(_.pure[F]))
      .get

    val email = req.email.getOrElse(orig.email)
    val name = req.name.getOrElse(orig.userName)
    val role = req.role.map(Role(_)).getOrElse(orig.role)
    val modified: F[Either[UserValidationError, User]] = for {
      hash <- newHash
      user = {
        if (identity.role == Role.Administrator)
          Either.right(User(name, email, Some(hash), role, orig.id))
        else if (identity.id == orig.id)
          Either.right(orig.copy(userName = name, email = email, hash = Some(hash)))
        else Either.left[UserValidationError, User](UserForbiddenError)
      }
    } yield user

    EitherT(modified)
  }

  private def updateEndpoint(userService: UserService[F], cryptService: PasswordHasher[F, BCrypt])
    : TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AuthenticatedRequired) {
      case secured @ PUT -> Root / "users" / name asAuthed user =>
        val req = secured.request
        val action: EitherT[F, NonEmptyList[UserValidationError], User] = for {
          updateReq <- EitherT.liftF(req.as[UserUpdateRequest])
          origUser <- userService.getUserByName(name).leftMap(NonEmptyList.one)
          updated <- getModifiedUser(user, cryptService, origUser, updateReq).leftMap(
            NonEmptyList.one)
          result <- updateReq.password match {
            case Some(password) => userService.updateCheckingPassword(updated, password)
            case None => userService.update(updated)
          }
        } yield result

        responseBuilder.buildList(action.value)
    }

  private def searchByNameEndpoint(userService: UserService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "users" / userName =>
        responseBuilder.build(
          userService.getUserByName(userName).leftWiden[UserValidationError].value)
    }

  private def getUserEndpoint(userService: UserService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" / LongVar(id) =>
        responseBuilder.build(userService.getUser(id).leftWiden[UserValidationError].value)
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
    loginEndpoint(authService) <+>
      signupEndpoint(userService, cryptService) <+>
      searchByNameEndpoint(userService) <+>
      getUserEndpoint(userService) <+>
      authService.Auth.liftWithFallthrough(
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
