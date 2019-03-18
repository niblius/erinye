package io.github.niblius.erinye.infrastructure.doobie

import cats._
import cats.data._
import cats.implicits._
import doobie._
import doobie.implicits._
import io.github.niblius.erinye.domain.comments.{Comment, CommentRepositoryAlgebra}

private object CommentSQL {
  def insert(comment: Comment): Update0 = sql"""
    INSERT INTO COMMENTS (
      USER_ID,
      ARTICLE_ID,
      BODY,
      ANONYM_NAME,
      DATE_CREATED,
      DATE_EDITED
    )
    VALUES (
      ${comment.userId},
      ${comment.articleId},
      ${comment.body},
      ${comment.anonymName},
      ${comment.dateCreated},
      ${comment.dateEdited}
    )
  """.update

  def update(comment: Comment, id: Long): Update0 = sql"""
    UPDATE COMMENTS
    SET USER_ID = ${comment.userId},
        ARTICLE_ID = ${comment.articleId},
        BODY = ${comment.body},
        ANUNYM_NAME = ${comment.anonymName},
        DATE_CREATED = ${comment.dateCreated},
        DATE_EDITED = ${comment.dateEdited},
    WHERE ID = $id
  """.update

  def select(id: Long): Query0[Comment] = sql"""
    SELECT ID,
           USER_ID,
           ARTICLE_ID,
           BODY,
           ANONYM_NAME,
           DATE_CREATED,
           DATE_EDITED
    FROM COMMENTS
    WHERE ID = $id
  """.query

  def delete(id: Long): Update0 = sql"""
    DELETE FROM COMMENTS WHERE ID = $id
  """.update

  def selectAll(articleId: Long): Query0[Comment] = sql"""
    SELECT
      ID,
      USER_ID,
      ARTICLE_ID,
      BODY,
      ANONYM_NAME,
      DATE_CREATED,
      DATE_EDITED
    FROM COMMENTS
    WHERE ARTICLE_ID = $articleId
    ORDER BY DATE_CREATED
  """.query
}

class DoobieCommentRepositoryInterpreter[F[_]: Monad](val xa: Transactor[F])
    extends CommentRepositoryAlgebra[F] {
  import CommentSQL._

  def create(comment: Comment): F[Comment] =
    insert(comment)
      .withUniqueGeneratedKeys[Long]("ID")
      .map(id => comment.copy(id = id.some))
      .transact(xa)

  def update(comment: Comment): F[Option[Comment]] =
    OptionT
      .fromOption[ConnectionIO](comment.id)
      .semiflatMap(id => CommentSQL.update(comment, id).run.as(comment))
      .value
      .transact(xa)

  def get(id: Long): F[Option[Comment]] = select(id).option.transact(xa)

  def delete(id: Long): F[Option[Comment]] =
    OptionT(get(id))
      .semiflatMap(comment => CommentSQL.delete(id).run.transact(xa).as(comment))
      .value

  def list(articleId: Long): F[List[Comment]] =
    selectAll(articleId).to[List].transact(xa)
}

object DoobieCommentRepositoryInterpreter {
  def apply[F[_]: Monad](xa: Transactor[F]): DoobieCommentRepositoryInterpreter[F] =
    new DoobieCommentRepositoryInterpreter(xa)
}
