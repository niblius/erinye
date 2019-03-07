package io.github.niblius.erinye.domain.users

import cats.MonadError
import cats.effect.Sync
import io.circe.Encoder
import io.circe.generic.auto._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.jsonOf
import org.http4s.circe.jsonEncoderOf
import tsec.authorization.AuthorizationInfo

case class User(
    userName: String,
    email: String,
    hash: Option[String],
    role: Role = Role.PlainUser,
    id: Option[Long] = None
)

object User {
  implicit def userDecoder[F[_]: Sync]: EntityDecoder[F, User] = jsonOf

  implicit val userEncoder: Encoder[User] = Encoder.forProduct4(
    "userName",
    "email",
    "role",
    "id"
  )(u => (u.userName, u.email, u.role.roleRepr, u.id))

  implicit def userEntityEncoder[F[_]: Sync]: EntityEncoder[F, User] =
    jsonEncoderOf[F, User]

  implicit def authRole[F[_]](
      implicit F: MonadError[F, Throwable]): AuthorizationInfo[F, Role, User] =
    (u: User) => F.pure(u.role)
}
