package io.github.niblius.erinye.domain.comments
import cats.data.{EitherT, ValidatedNel}

trait CommentValidationAlgebra[F[_]] {
  def exists(commentId: Long): EitherT[F, CommentNotFoundError.type, Comment]
  def validateBody(body: String): EitherT[F, InvalidCommentBody.type, Unit]
  def validateArticle(articleId: Long): EitherT[F, CommentArticleNotFound.type, Unit]
  def validateAnonymousName(name: Option[String]): EitherT[F, InvalidAnonymousName.type, Unit]
  def validateComment(comment: Comment): F[ValidatedNel[CommentValidationError, Unit]]
  def validateAnonymousComment(comment: Comment): F[ValidatedNel[CommentValidationError, Unit]]
}
