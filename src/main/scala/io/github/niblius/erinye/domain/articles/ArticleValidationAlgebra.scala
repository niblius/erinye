package io.github.niblius.erinye.domain.articles

import cats.data.EitherT
import io.github.niblius.erinye.domain.ArticleNotFoundError

trait ArticleValidationAlgebra[F[_]] {
  def exists(articleId: Option[Long]): EitherT[F, ArticleNotFoundError.type, Unit]
}
