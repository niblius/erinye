package io.github.niblius.erinye.infrastructure.endpoint

import cats.effect.Sync
import org.http4s.circe._
import io.circe.generic.auto._
import io.github.niblius.erinye.domain.users.User
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.jsonOf
import tsec.passwordhashers.PasswordHash

package object user {
  implicit def loginRequestEnc[F[_]: Sync]: EntityEncoder[F, LoginRequest] = jsonEncoderOf
  implicit def loginReqDec[F[_]: Sync]: EntityDecoder[F, LoginRequest] = jsonOf
  final case class LoginRequest(userName: String, password: String)

  implicit def signupRequestEnc[F[_]: Sync]: EntityEncoder[F, SignupRequest] = jsonEncoderOf
  implicit def signupReqDec[F[_]: Sync]: EntityDecoder[F, SignupRequest] = jsonOf
  final case class SignupRequest(userName: String, email: String, password: String) {
    def asUser[A](hashedPassword: PasswordHash[A]): User = User(
      userName,
      email,
      Some(hashedPassword.toString)
    )
  }

  implicit def userUpdateRequestEnc[F[_]: Sync]: EntityEncoder[F, UserUpdateRequest] = jsonEncoderOf
  implicit def userUpdateRequestDec[F[_]: Sync]: EntityDecoder[F, UserUpdateRequest] = jsonOf
  final case class UserUpdateRequest(
      name: Option[String],
      email: Option[String],
      password: Option[String],
      role: Option[String]
  )

}
