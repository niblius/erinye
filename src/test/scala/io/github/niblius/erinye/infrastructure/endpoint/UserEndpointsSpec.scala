package io.github.niblius.erinye
package infrastructure.endpoint.user

import cats.Id
import cats.effect._
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
import org.http4s.headers.Authorization
import tsec.authentication.{AugmentedJWT, SecuredRequest}
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256

import infrastructure.ErinyeArbitraries
import infrastructure.doobie.DoobieUserRepositoryInterpreter
import infrastructure.TokenRepositoryInterpreter
import infrastructure.endpoint._
import User._
import infrastructure.endpoint.user._

class UserEndpointsSpec
    extends FunSuite
    with Matchers
    with PropertyChecks
    with ErinyeArbitraries
    with Http4sDsl[IO]
    with Http4sClientDsl[IO] {

  class Deps {
    val userRepo =
      DoobieUserRepositoryInterpreter[IO](endpoint.testTransactor)
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
  }

  test("create user") {
    val d = new Deps
    import d._

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
    val d = new Deps
    import d._

    forAll { (userSignup: SignupRequest, newEmail: NonEmptyString) =>
      (for {
        createRequest <- POST(userSignup, Uri.uri("/users"))
        createResponse <- userHttpService.run(createRequest)
        createdUser <- createResponse.as[User]
        loginRequest <- POST(
          LoginRequest(userSignup.userName, userSignup.password),
          Uri.unsafeFromString("/login"))
        loginResponse <- userHttpService.run(loginRequest)
        tokenData <- loginResponse.as[Token]
        userToUpdate = createdUser.copy(email = newEmail.str)
        updateUser = Request(
          method = PUT,
          uri = Uri.unsafeFromString(s"/users/${createdUser.userName}"))
          .withEntity(userToUpdate)(User.userEntityEncoder)
          .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, tokenData.encoded)))
        updateResponse <- userHttpService.run(updateUser)
        updatedUser <- updateResponse.as[User]
      } yield {
        updateResponse.status shouldEqual Ok
        updatedUser.email shouldEqual newEmail.str
        createdUser.id shouldEqual updatedUser.id
      }).unsafeRunSync
    }
  }

  test("get user by userName") {
    val d = new Deps
    import d._

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
    val d = new Deps
    import d._

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
