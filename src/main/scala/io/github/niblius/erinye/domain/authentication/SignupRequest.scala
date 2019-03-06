package io.github.niblius.erinye.domain.authentication

import cats.effect.Sync
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import tsec.passwordhashers.PasswordHash
import io.circe.generic.auto._
import io.github.niblius.erinye.domain.users.User

final case class SignupRequest(userName: String, email: String, password: String) {
  def asUser[A](hashedPassword: PasswordHash[A]): User = User(
    userName,
    email,
    Some(hashedPassword.toString)
  )
}

object SignupRequest {
  implicit def signupReqDecoder[F[_]: Sync]: EntityDecoder[F, SignupRequest] = jsonOf
}
