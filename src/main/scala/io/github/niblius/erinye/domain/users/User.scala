package io.github.niblius.erinye.domain.users

import cats.{Eq, MonadError}
import cats.effect.Sync
import cats.implicits._
import io.circe.Encoder
import io.circe.generic.auto._
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import tsec.authorization.{AuthGroup, AuthorizationInfo, SimpleAuthEnum}

case class User(
    userName: String,
    email: String,
    hash: Option[String],
    role: Role = Role.PlainUser,
    id: Option[Long] = None,
)

object User {
  implicit def userDecoder[F[_]: Sync]: EntityDecoder[F, User] = jsonOf
  implicit val userEncoder: Encoder[User] = Encoder.forProduct4(
    "userName",
    "email",
    "role",
    "id"
  )(u => (u.userName, u.email, u.role.roleRepr, u.id))

  implicit def authRole[F[_]](
      implicit F: MonadError[F, Throwable]): AuthorizationInfo[F, Role, User] =
    (u: User) => F.pure(u.role)
}

sealed case class Role(roleRepr: String)

object Role extends SimpleAuthEnum[Role, String] {

  val Administrator: Role = Role("Administrator")
  val PlainUser: Role = Role("PlainUser")

  implicit val E: Eq[Role] = Eq.fromUniversalEquals[Role]

  def getRepr(t: Role): String = t.roleRepr

  protected val values: AuthGroup[Role] = AuthGroup(Administrator, PlainUser)
}
