package io.github.niblius.erinye.domain.articles

import cats._
import cats.data._
import cats.effect.Sync
import cats.implicits._

import scala.language.higherKinds

class ArticleService[F[_]: Sync](
    repository: ArticleRepositoryAlgebra[F],
    validation: ArticleValidationAlgebra[F]) {

  def create(article: Article): EitherT[F, NonEmptyList[ArticleValidationError], Article] =
    for {
      _ <- EitherT(validation.validate(article).map(_.toEither))
      saved <- EitherT.right(repository.create(article))
    } yield saved

  def update(article: Article)(
      implicit M: Monad[F]): EitherT[F, NonEmptyList[ArticleValidationError], Article] =
    for {
      _ <- EitherT(validation.validate(article).map(_.toEither))
      saved <- EitherT.fromOptionF[F, NonEmptyList[ArticleValidationError], Article](
        repository.update(article),
        NonEmptyList.one(ArticleNotFoundError))
    } yield saved

  def get(id: Long)(implicit M: Monad[F]): EitherT[F, ArticleNotFoundError.type, Article] =
    EitherT.fromOptionF(repository.get(id), ArticleNotFoundError)

  def delete(id: Long)(implicit M: Monad[F]): EitherT[F, ArticleNotFoundError.type, Article] =
    EitherT.fromOptionF(repository.delete(id), ArticleNotFoundError)

  def list(pageSize: Int, offset: Int): F[List[Article]] =
    repository.list(pageSize, offset)

  def findByTag(tags: NonEmptyList[String]): F[List[Article]] =
    repository.findByTag(tags)

  def searchByTitle(title: String): F[List[Article]] =
    repository.searchByTitle(title)
}

object ArticleService {
  def apply[F[_]: Sync](
      repository: ArticleRepositoryAlgebra[F],
      validation: ArticleValidationAlgebra[F]) =
    new ArticleService[F](repository, validation)
}
