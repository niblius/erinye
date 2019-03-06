package io.github.niblius.erinye.domain.authentication

import cats.implicits._
import cats.data._
import cats.effect.Sync
import io.github.niblius.erinye.domain.UserAuthenticationFailedError
import io.github.niblius.erinye.domain.users.{User, UserService}
import tsec.authentication._
import tsec.common.{SecureRandomId, Verified}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import tsec.passwordhashers.{PasswordHash, PasswordHasher}
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.duration._

class AuthService[F[_]: Sync](
    userService: UserService[F],
    jwtStore: BackingStore[F, SecureRandomId, AugmentedJWT[HMACSHA256, Long]],
    signingKey: MacSigningKey[HMACSHA256],
    cryptService: PasswordHasher[F, BCrypt]
) {
  // TODO: Use redis
  val userBackingStore: BackingStore[F, Long, User] = new BackingStore[F, Long, User] {
    // should never be called
    override def put(user: User): F[User] = ???
    override def update(user: User): F[User] = ???
    override def delete(id: Long): F[Unit] = ???

    override def get(id: Long): OptionT[F, User] = userService.getUser(id).toOption
  }

  type AJWT = AugmentedJWT[HMACSHA256, Long]
  val Auth: SecuredRequestHandler[F, Long, User, AJWT] =
    SecuredRequestHandler(
      JWTAuthenticator.backed.inBearerToken(28.days, None, jwtStore, userBackingStore, signingKey))

  def login(loginRequest: LoginRequest): EitherT[F, UserAuthenticationFailedError, TokenResponse] =
    for {
      credentials <- EitherT.rightT[F, UserAuthenticationFailedError](loginRequest)
      name = credentials.userName
      user <- userService.getUserByName(name).leftMap(_ => UserAuthenticationFailedError(name))
      checkResult <- EitherT.liftF(
        cryptService.checkpw(credentials.password, PasswordHash[BCrypt](user.hash.get)))
      augJWT <- if (checkResult == Verified)
        EitherT.liftF[F, UserAuthenticationFailedError, AJWT](
          Auth.authenticator.create(user.id.get))
      else
        EitherT.leftT[F, AJWT](UserAuthenticationFailedError(name))
      token = JWTMac.toEncodedString[F, HMACSHA256](augJWT.jwt)
      expiry = augJWT.expiry
    } yield TokenResponse(token, expiry)
}

object AuthService {
  def apply[F[_]: Sync](
      userService: UserService[F],
      jwtStore: BackingStore[F, SecureRandomId, AugmentedJWT[HMACSHA256, Long]],
      signingKey: MacSigningKey[HMACSHA256],
      cryptService: PasswordHasher[F, BCrypt]
  ): AuthService[F] = new AuthService(userService, jwtStore, signingKey, cryptService)
}
