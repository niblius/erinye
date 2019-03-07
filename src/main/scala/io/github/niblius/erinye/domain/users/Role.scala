package io.github.niblius.erinye.domain.users

import cats.Eq
import cats.implicits._
import tsec.authorization.{AuthGroup, SimpleAuthEnum}

sealed case class Role(roleRepr: String)

object Role extends SimpleAuthEnum[Role, String] {

  val Administrator: Role = Role("Administrator")
  val PlainUser: Role = Role("PlainUser")

  implicit val E: Eq[Role] = Eq.fromUniversalEquals[Role]

  def getRepr(t: Role): String = t.roleRepr

  protected val values: AuthGroup[Role] = AuthGroup(Administrator, PlainUser)

  def apply(roleRepr: String): Role =
    if (roleRepr == "Administrator")
      Administrator
    else
      PlainUser
}
