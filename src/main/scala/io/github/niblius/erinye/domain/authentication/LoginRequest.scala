package io.github.niblius.erinye
package domain.authentication

import cats.effect.Sync
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import io.circe.generic.auto._

final case class LoginRequest(userName: String, password: String)

object LoginRequest {
  implicit def loginReqDecoder[F[_]: Sync]: EntityDecoder[F, LoginRequest] = jsonOf
}
