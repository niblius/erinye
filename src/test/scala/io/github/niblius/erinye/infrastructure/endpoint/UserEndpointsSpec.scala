package io.github.niblius.erinye

import cats.Id
import cats.effect._
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import org.http4s.client.dsl.Http4sClientDsl
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import tsec.passwordhashers.jca.BCrypt
import domain.users._
import domain.authentication._
import org.http4s.headers.Authorization
import tsec.authentication.AugmentedJWT
import tsec.common.SecureRandomId
import tsec.mac.jca.HMACSHA256
import infrastructure.ErinyeArbitraries
import infrastructure.doobie.DoobieUserRepositoryInterpreter
import infrastructure.TokenRepositoryInterpreter
import infrastructure.endpoint._
import User._
import org.http4s.{AuthScheme, Credentials, EntityEncoder, Request, Response, Uri}

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

    def signUp(userSignup: SignupRequest): IO[Response[IO]] =
      for {
        request <- POST(userSignup, Uri.uri("/users"))
        response <- userHttpService.run(request)
      } response

    def signInAndLogIn(userSignup: SignupRequest): IO[(Response[IO], Response[IO])] =
      for {
        createResponse <- signUp(userSignup)
        loginRequest <- POST(
          LoginRequest(userSignup.userName, userSignup.password),
          Uri.unsafeFromString("/login"))
        loginResponse <- userHttpService.run(loginRequest)
      } yield (createResponse, loginResponse)
  }

  test("sign up user") {
    val d = new Deps
    import d._

    forAll { userSignup: SignupRequest =>
      signUp(userSignup)
        .map(response => response.status shouldEqual Ok)
        .unsafeRunSync
    }
  }

  test("update user") {
    val d = new Deps
    import d._

    forAll { (userSignup: SignupRequest, userUpdateRequest: UserUpdateRequest) =>
      (for {
        (createResponse, loginResponse) <- signInAndLogIn(userSignup)
        createdUser <- createResponse.as[User]
        tokenData <- loginResponse.as[Token]
        updateUser = Request(
          method = PUT,
          uri = Uri.unsafeFromString(s"/users/${createdUser.userName}"))
          .withEntity(userUpdateRequest)
          .withHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, tokenData.encoded)))
        updateResponse <- userHttpService.run(updateUser)
        updatedUser <- updateResponse.as[User]
        reLoginRequest <- POST(
          LoginRequest(userUpdateRequest.name.get, userUpdateRequest.password.get),
          Uri.unsafeFromString("/login"))
        reLoginResponse <- userHttpService.run(reLoginRequest)
      } yield {
        createResponse.status shouldEqual Ok
        loginResponse.status shouldEqual Ok
        updateResponse.status shouldEqual Ok
        reLoginResponse.status shouldEqual Ok

        updatedUser.email shouldEqual userUpdateRequest.email
        updatedUser.userName shouldEqual userUpdateRequest.name
        createdUser.id shouldEqual updatedUser.id
      }).unsafeRunSync
    }
  }

  test("get user by userName") {
    val d = new Deps
    import d._

    forAll { userSignup: SignupRequest =>
      (for {
        createResponse <- signUp(userSignup)
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
        deleteResponse.status shouldEqual Unauthorized
        getResponse.status shouldEqual Ok
      }).unsafeRunSync
    }
  }
}
