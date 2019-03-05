package io.github.niblius.erinye.domain.articles

import cats._
import cats.data._
import cats.implicits._
import io.github.niblius.erinye.domain.ArticleNotFoundError

import scala.language.higherKinds

class ArticleService[F[_]](repository: ArticleRepositoryAlgebra[F], validation: ArticleValidationAlgebra[F]) {

  def create(article: Article)(implicit M: Monad[F]): F[Article] =
    repository.create(article)

  def update(article: Article)(implicit M: Monad[F]): EitherT[F, ArticleNotFoundError.type, Article] = for {
    _ <- validation.exists(article.id)
    saved <- EitherT.fromOptionF(repository.update(article), ArticleNotFoundError)
  } yield saved

  def get(id: Long)(implicit M: Monad[F]): EitherT[F, ArticleNotFoundError.type, Article] =
    EitherT.fromOptionF(repository.get(id), ArticleNotFoundError)

  def delete(id: Long)(implicit M: Monad[F]): F[Unit] =
    repository.delete(id).as(())

  def list(pageSize: Int, offset: Int): F[List[Article]] =
    repository.list(pageSize, offset)

  def findByTag(tags: NonEmptyList[String]): F[List[Article]] =
    repository.findByTag(tags)

  def searchByTitle(title: String): F[List[Article]] =
    repository.searchByTitle(title)
}

object ArticleService {
  def apply[F[_]: Monad](repository: ArticleRepositoryAlgebra[F], validation: ArticleValidationAlgebra[F]) =
    new ArticleService[F](repository, validation)
}