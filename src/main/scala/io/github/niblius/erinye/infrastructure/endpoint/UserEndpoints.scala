package io.github.niblius.erinye.infrastructure.endpoint

import cats.data.{EitherT, NonEmptyList}
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
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

final case class SignupRequest(userName: String, email: String, password: String) {
  def asUser[A](hashedPassword: PasswordHash[A]): User = User(
    userName,
    email,
    Some(hashedPassword.toString)
  )
}

final case class UserUpdateRequest(
    name: Option[String],
    email: Option[String],
    password: Option[String],
    role: Option[String]
)

class UserEndpoints[F[_]: Effect] extends Http4sDsl[F] {
  implicit val loginRequestEnc: EntityEncoder[F, LoginRequest] = jsonEncoderOf
  implicit val loginReqDec: EntityDecoder[F, LoginRequest] = jsonOf
  implicit val signupRequestEnc: EntityEncoder[F, SignupRequest] = jsonEncoderOf
  implicit val signupReqDec: EntityDecoder[F, SignupRequest] = jsonOf
  implicit val userUpdateRequestEnc: EntityEncoder[F, UserUpdateRequest] = jsonEncoderOf
  implicit val userUpdateRequestDec: EntityDecoder[F, UserUpdateRequest] = jsonOf

  private val responseBuilder = ResponseBuilder[F, UserValidationError](
    {
      case UserNotFoundError => NotFound(_)
      case UserAlreadyExistsError =>
        Conflict(_)
      case UserForbiddenError => Forbidden(_)
      case _ => BadRequest(_)
    },
    errors => BadRequest(errors.asJson),
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

    val modified: F[Either[UserValidationError, User]] =
      if (identity.role == Role.Administrator)
        for {
          hash <- newHash
          email = req.email.getOrElse(orig.email)
          name = req.name.getOrElse(orig.userName)
          role = req.role.map(Role(_)).getOrElse(orig.role)
        } yield Either.right(User(name, email, Some(hash), role, orig.id))
      else if (identity.id == orig.id)
        for {
          hash <- newHash
          email = req.email.getOrElse(orig.email)
        } yield Either.right(orig.copy(email = email, hash = Some(hash)))
      else
        Either.left[UserValidationError, User](UserForbiddenError).pure[F]

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
