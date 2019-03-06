package io.github.niblius.erinye
package infrastructure.endpoint

import cats.Id
import cats.effect._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl._
import org.http4s.circe._
import org.http4s.client.dsl.Http4sClientDsl
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import tsec.passwordhashers.jca.BCrypt
import domain.users._
import domain.authentication._
import infrastructure.ErinyeArbitraries
import infrastructure.doobie.DoobieUserRepositoryInterpreter
import endpoint._
import infrastructure.TokenRepositoryInterpreter
import tsec.authentication.{AugmentedJWT, SecuredRequest}
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256

class UserEndpointsSpec
    extends FunSuite
    with Matchers
    with PropertyChecks
    with ErinyeArbitraries
    with Http4sDsl[IO]
    with Http4sClientDsl[IO] {

  // TODO: we already have some of those
  implicit val userEnc: EntityEncoder[IO, User] = jsonEncoderOf
  implicit val userDec: EntityDecoder[IO, User] = jsonOf
  implicit val tokenDec: EntityDecoder[IO, TokenResponse] = jsonOf
  implicit val loginRequestEnc: EntityEncoder[IO, LoginRequest] = jsonEncoderOf
  implicit val signupRequestEnc: EntityEncoder[IO, SignupRequest] = jsonEncoderOf
  implicit val signupRequestDec: EntityDecoder[IO, SignupRequest] = jsonOf // Why do we need this?

  val userRepo = DoobieUserRepositoryInterpreter[IO](testTransactor)
  val userValidation = UserValidationInterpreter[IO](userRepo)
  val userService = UserService[IO](userRepo, userValidation)
  val cryptService = BCrypt.syncPasswordHasher[IO]
  val tokenStore =
    TokenRepositoryInterpreter[IO, SecureRandomId, AugmentedJWT[HMACSHA256, Long]](s =>
      SecureRandomId.coerce(s.id))
  val signingKey = HMACSHA256.generateKey[Id]
  val authService = AuthService[IO](userService, tokenStore, signingKey, cryptService)
  val userHttpService =
    UserEndpoints.endpoints(userService, BCrypt.syncPasswordHasher[IO], authService).orNotFound

  test("create user") {
    forAll { userSignup: SignupRequest =>
      (for {
        request <- POST(userSignup, Uri.uri("/users"))
        response <- userHttpService.run(request)
      } yield {
        response.status shouldEqual Ok
      }).unsafeRunSync
    }
  }

  test("update user") {
    forAll { (userSignup: SignupRequest, newEmail: String) =>
      (for {
        createRequest <- POST(userSignup, Uri.uri("/users"))
        createResponse <- userHttpService.run(createRequest)
        createdUser <- createResponse.as[User]
        loginRequest <- POST(
          LoginRequest(userSignup.userName, userSignup.password),
          Uri.unsafeFromString("/login"))
        loginResponse <- userHttpService.run(loginRequest)
        token <- loginResponse.as[TokenResponse]
        dummySreq <- SecuredRequest[IO, User, Long](
          Request[IO](),
          User("asd", "email@asd.asd", Some("ASd")),
          0).
        _ <- IO(println(dummySreq))
        userToUpdate = createdUser.copy(email = newEmail)
        updateUser <- PUT(userToUpdate, Uri.unsafeFromString(s"/users/${createdUser.userName}"))
        updateResponse <- userHttpService.run(updateUser)
        updatedUser <- updateResponse.as[User]
      } yield {
        updateResponse.status shouldEqual Ok
        updatedUser.email shouldEqual newEmail
        createdUser.id shouldEqual updatedUser.id
      }).unsafeRunSync
    }
  }

  test("get user by userName") {
    forAll { userSignup: SignupRequest =>
      (for {
        createRequest <- POST(userSignup, Uri.uri("/users"))
        createResponse <- userHttpService.run(createRequest)
        createdUser <- createResponse.as[User]
        getRequest <- GET(Uri.unsafeFromString(s"/users/${createdUser.userName}"))
        getResponse <- userHttpService.run(getRequest)
        getUser <- getResponse.as[User]
      } yield {
        getResponse.status shouldEqual Ok
        createdUser.userName shouldEqual getUser.userName
      }).unsafeRunSync
    }
  }

  test("delete user by userName") {
    forAll { userSignup: SignupRequest =>
      (for {
        createRequest <- POST(userSignup, Uri.uri("/users"))
        createResponse <- userHttpService.run(createRequest)
        createdUser <- createResponse.as[User]
        deleteRequest <- DELETE(Uri.unsafeFromString(s"/users/${createdUser.userName}"))
        deleteResponse <- userHttpService.run(deleteRequest)
        getRequest <- GET(Uri.unsafeFromString(s"/users/${createdUser.userName}"))
        getResponse <- userHttpService.run(getRequest)
      } yield {
        createResponse.status shouldEqual Ok
        deleteResponse.status shouldEqual Ok
        getResponse.status shouldEqual NotFound
      }).unsafeRunSync
    }
  }
}
