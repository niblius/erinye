package io.github.niblius.erinye.domain.articles

import cats.data._

trait ArticleValidationAlgebra[F[_]] {
  def exists(articleId: Long): EitherT[F, ArticleValidationError, Article]
  def validateTitle(title: String): EitherT[F, ArticleValidationError, Unit]
  def validateDesc(description: String): EitherT[F, ArticleValidationError, Unit]
  def validateContent(content: String): EitherT[F, ArticleValidationError, Unit]
  def validateTags(tags: Set[String]): EitherT[F, ArticleValidationError, Unit]
  def validateAuthor(userId: Long): EitherT[F, ArticleValidationError, Unit]
  def validate(article: Article): F[ValidatedNel[ArticleValidationError, Unit]]
}
