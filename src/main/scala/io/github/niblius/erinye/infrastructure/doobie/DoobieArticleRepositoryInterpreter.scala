package io.github.niblius.erinye.infrastructure.doobie

import cats._
import cats.data._
import cats.implicits._
import doobie._
import doobie.implicits._
import SQLPagination._
import io.github.niblius.erinye.domain.articles.ArticleRepositoryAlgebra
import io.github.niblius.erinye.domain.articles.Article

private object ArticleSQL {
  /* This is used to marshal our sets of strings */
  implicit val SetStringMeta: Meta[Set[String]] =
    Meta[String].imap(_.split(',').toSet)(_.mkString(","))

  def insert(article: Article): Update0 = sql"""
    INSERT INTO ARTICLES (TITLE, DESCRIPTION, CONTENT, TAGS, USER_ID, DATE_CREATED)
    VALUES (${article.title}, ${article.description}, ${article.content}, ${article.tags}, ${article.userId}, ${article.dateCreated})
  """.update

  def update(article: Article, id: Long): Update0 = sql"""
    UPDATE ARTICLES
    SET TITLE = ${article.title},
        DESCRIPTION = ${article.description},
        CONTENT = ${article.content},
        TAGS = ${article.tags},
        USER_ID = ${article.userId},
        DATE_CREATED = ${article.dateCreated}
    WHERE id = $id
  """.update

  def select(id: Long): Query0[Article] = sql"""
    SELECT TITLE, DESCRIPTION, CONTENT, TAGS, USER_ID, DATE_CREATED, ID
    FROM ARTICLES
    WHERE ID = $id
  """.query

  def delete(id: Long): Update0 = sql"""
    DELETE FROM ARTICLES WHERE ID = $id
  """.update

  def selectAll: Query0[Article] = sql"""
    SELECT TITLE, DESCRIPTION, CONTENT, TAGS, USER_ID, DATE_CREATED, ID
    FROM ARTICLES
    ORDER BY DATE_CREATED
  """.query

  def selectTagLikeString(tags: NonEmptyList[String]): Query0[Article] = {
    /* Handle dynamic construction of query based on multiple parameters */

    /* To piggyback off of comment of above reference about tags implementation, findByTag uses LIKE for partial matching
    since tags is (currently) implemented as a comma-delimited string */
    // TODO: Security (!!!) issue: tag request may be vulnerable to sql injection
    val tagLikeString: String = tags.toList.mkString("TAGS LIKE '%", "%' OR TAGS LIKE '%", "%'")
    (sql"""
       SELECT TITLE, DESCRIPTION, CONTENT, TAGS, USER_ID, DATE_CREATED, ID
       FROM ARTICLES
       WHERE """ ++ Fragment.const(tagLikeString))
      .query[Article]
  }

  // TODO: check on SQL-injection here too
  def selectTitleLikeString(title: String): Query0[Article] =
    sql"""
      SELECT TITLE, DESCRIPTION, CONTENT, TAGS, USER_ID, DATE_CREATED, ID
      FROM ARTICLES
      WHERE TITLE LIKE '%$title%'"""
      .query[Article]
}

class DoobieArticleRepositoryInterpreter[F[_]: Monad](val xa: Transactor[F])
    extends ArticleRepositoryAlgebra[F] {
  import ArticleSQL._

  def create(article: Article): F[Article] =
    insert(article)
      .withUniqueGeneratedKeys[Long]("ID")
      .map(id => article.copy(id = id.some))
      .transact(xa)

  def update(article: Article): F[Option[Article]] =
    OptionT
      .fromOption[ConnectionIO](article.id)
      .semiflatMap(id => ArticleSQL.update(article, id).run.as(article))
      .value
      .transact(xa)

  def get(id: Long): F[Option[Article]] = select(id).option.transact(xa)

  def delete(id: Long): F[Option[Article]] =
    OptionT(get(id))
      .semiflatMap(article => ArticleSQL.delete(id).run.transact(xa).as(article))
      .value

  def list(pageSize: Int, offset: Int): F[List[Article]] =
    paginate(pageSize, offset)(selectAll).to[List].transact(xa)

  def findByTag(tags: NonEmptyList[String]): F[List[Article]] =
    selectTagLikeString(tags).to[List].transact(xa)

  def searchByTitle(title: String): F[List[Article]] =
    selectTitleLikeString(title).to[List].transact(xa)
}

object DoobieArticleRepositoryInterpreter {
  def apply[F[_]: Monad](xa: Transactor[F]): DoobieArticleRepositoryInterpreter[F] =
    new DoobieArticleRepositoryInterpreter(xa)
}
