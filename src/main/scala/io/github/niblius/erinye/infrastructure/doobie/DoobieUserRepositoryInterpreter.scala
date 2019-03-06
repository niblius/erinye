package io.github.niblius.erinye.infrastructure.doobie

import cats._
import cats.data.OptionT
import cats.implicits._
import doobie._
import doobie.implicits._
import io.github.niblius.erinye.domain.users.{User, UserRepositoryAlgebra}

private object UserSQL {
  def insert(user: User): Update0 = sql"""
    INSERT INTO USERS (USER_NAME, EMAIL, HASH, ROLE)
    VALUES (${user.userName}, ${user.email}, ${user.hash}, ${user.role.roleRepr})
  """.update

  def update(user: User, id: Long): Update0 = sql"""
    UPDATE USERS
    SET EMAIL = ${user.email}, HASH = ${user.hash}, ROLE = ${user.role.roleRepr}
    WHERE ID = $id
  """.update

  def select(userId: Long): Query0[User] = sql"""
    SELECT USER_NAME, EMAIL, HASH, ROLE, ID
    FROM USERS
    WHERE ID = $userId
  """.query

  def byUserName(userName: String): Query0[User] = sql"""
    SELECT USER_NAME, EMAIL, HASH, ROLE, ID
    FROM USERS
    WHERE USER_NAME = $userName
  """.query[User]

  def delete(userId: Long): Update0 = sql"""
    DELETE FROM USERS WHERE ID = $userId
  """.update
}

class DoobieUserRepositoryInterpreter[F[_]: Monad](val xa: Transactor[F])
    extends UserRepositoryAlgebra[F] {

  import UserSQL._

  def create(user: User): F[User] =
    insert(user).withUniqueGeneratedKeys[Long]("ID").map(id => user.copy(id = id.some)).transact(xa)

  def update(user: User): F[Option[User]] =
    OptionT
      .fromOption[F](user.id)
      .semiflatMap { id =>
        UserSQL.update(user, id).run.transact(xa).as(user)
      }
      .value

  def get(userId: Long): F[Option[User]] = select(userId).option.transact(xa)

  def findByUserName(userName: String): F[Option[User]] = byUserName(userName).option.transact(xa)

  def delete(userId: Long): F[Option[User]] =
    OptionT(get(userId)).semiflatMap(user => UserSQL.delete(userId).run.transact(xa).as(user)).value

  def deleteByUserName(userName: String): F[Option[User]] =
    OptionT(findByUserName(userName)).mapFilter(_.id).flatMapF(delete).value
}

object DoobieUserRepositoryInterpreter {
  def apply[F[_]: Monad](xa: Transactor[F]): DoobieUserRepositoryInterpreter[F] =
    new DoobieUserRepositoryInterpreter(xa)
}
