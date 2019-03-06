package io.github.niblius.erinye
package infrastructure.endpoint

import cats.data.EitherT
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpRoutes

import scala.language.higherKinds
import domain._
import domain.users._
import domain.authentication._
import tsec.passwordhashers.PasswordHasher
import tsec.passwordhashers.jca.BCrypt

class UserEndpoints[F[_]: Effect] extends Http4sDsl[F] {
  private def loginEndpoint(
      userService: UserService[F],
      authService: AuthService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "login" =>
        val action = for {
          loginReq <- EitherT.liftF(req.as[LoginRequest])
          resp <- authService.login(loginReq)
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

  private def updateEndpoint(userService: UserService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ PUT -> Root / "users" / name =>
        val action = for {
          user <- EitherT.liftF(req.as[User])
          origUser <- userService.getUserByName(name)
          updated = user.copy(userName = name, role = origUser.role)
          result <- userService.update(updated)
        } yield result

        action.value.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(UserNotFoundError) => NotFound("User not found")
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

  private def deleteUserEndpoint(userService: UserService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case DELETE -> Root / "users" / userName =>
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
      updateEndpoint(userService) <+>
      searchByNameEndpoint(userService) <+>
      deleteUserEndpoint(userService)
}

object UserEndpoints {
  def endpoints[F[_]: Effect](
      userService: UserService[F],
      cryptService: PasswordHasher[F, BCrypt],
      authService: AuthService[F]): HttpRoutes[F] =
    new UserEndpoints[F].endpoints(userService, cryptService, authService)
}
