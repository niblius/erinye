package io.github.niblius.erinye.domain.articles

import cats._
import cats.data.EitherT
import cats.implicits._
import io.github.niblius.erinye.domain.ArticleNotFoundError

class ArticleValidationInterpreter[F[_]: Monad](repository: ArticleRepositoryAlgebra[F])
  extends ArticleValidationAlgebra[F] {

  def exists(articleId: Option[Long]): EitherT[F, ArticleNotFoundError.type, Unit] =
    EitherT {
      articleId match {
        case Some(id) =>
          repository.get(id).map {
            case Some(_) => Right(())
            case _ => Left(ArticleNotFoundError)
          }
        case _ =>
          Either.left[ArticleNotFoundError.type, Unit](ArticleNotFoundError).pure[F]
      }
    }
}

object ArticleValidationInterpreter {
  def apply[F[_]: Monad](repository: ArticleRepositoryAlgebra[F]) =
    new ArticleValidationInterpreter[F](repository)
}
