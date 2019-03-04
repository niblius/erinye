package io.niblius.erinye.domain.articles

import io.niblius.erinye.domain.ArticleNotFoundError
import cats.data.EitherT

trait ArticleValidationAlgebra[F[_]] {
  def exists(articleId: Option[Long]): EitherT[F, ArticleNotFoundError.type, Unit]
}
