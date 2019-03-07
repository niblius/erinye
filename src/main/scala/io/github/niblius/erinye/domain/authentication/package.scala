package io.github.niblius.erinye.domain

import tsec.authentication.AugmentedJWT
import tsec.authorization.BasicRBAC
import tsec.mac.jca.HMACSHA256
import java.time.Instant
import cats.effect.Sync
import org.http4s.circe._
import org.http4s.{EntityDecoder, EntityEncoder}
import io.circe.generic.auto._

import io.github.niblius.erinye.domain.users.{Role, User}

package object authentication {
  def AdminRequired[F[_]: Sync]: BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Long]] =
    BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Long]](Role.Administrator)
  def AuthenticatedRequired[F[_]: Sync]: BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Long]] =
    BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Long]](Role.Administrator, Role.PlainUser)

  implicit def tokenEnc[F[_]: Sync]: EntityEncoder[F, Token] = jsonEncoderOf
  implicit def tokenDec[F[_]: Sync]: EntityDecoder[F, Token] = jsonOf
  case class Token(encoded: String, expiry: Instant)
}
