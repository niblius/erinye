package io.github.niblius.erinye.domain.comments

import cats.data.{EitherT, ValidatedNel}
import cats.effect.Sync
import cats.implicits._
import io.github.niblius.erinye.domain.articles.ArticleValidationAlgebra
import io.github.niblius.erinye.domain.users.UserValidationAlgebra

class CommentValidationInterpreter[F[_]: Sync](
    commentRepo: CommentRepositoryAlgebra[F],
    articleValidation: ArticleValidationAlgebra[F],
    userValidation: UserValidationAlgebra[F])
    extends CommentValidationAlgebra[F] {

  def exists(commentId: Long): EitherT[F, CommentNotFoundError.type, Comment] =
    EitherT(commentRepo.get(commentId).map(_.toRight(CommentNotFoundError)))

  def validateBody(body: String): EitherT[F, InvalidCommentBody.type, Unit] =
    EitherT.cond[F](!body.isBlank, (), InvalidCommentBody)

  def validateArticle(articleId: Long): EitherT[F, CommentArticleNotFound.type, Unit] =
    articleValidation.exists(articleId).bimap(_ => CommentArticleNotFound, _ => ())

  def validateAnonymousName(nameOpt: Option[String]): EitherT[F, InvalidAnonymousName.type, Unit] =
    for {
      name <- EitherT.fromOption[F](nameOpt, InvalidAnonymousName)
      _ <- EitherT.cond[F](!name.isBlank, (), InvalidAnonymousName)
    } yield ()

  def validateComment(comment: Comment): F[ValidatedNel[CommentValidationError, Unit]] =
    (
      validateBody(comment.body).leftWiden[CommentValidationError].toValidatedNel,
      validateArticle(comment.articleId).leftWiden[CommentValidationError].toValidatedNel
    ).mapN(_ |+| _)

  def validateAnonymousComment(comment: Comment): F[ValidatedNel[CommentValidationError, Unit]] =
    (
      validateBody(comment.body).leftWiden[CommentValidationError].toValidatedNel,
      validateArticle(comment.articleId).leftWiden[CommentValidationError].toValidatedNel,
      validateAnonymousName(comment.anonymName).leftWiden[CommentValidationError].toValidatedNel
    ).mapN(_ |+| _ |+| _)
}

object CommentValidationInterpreter {
  def apply[F[_]: Sync](
      commentRepo: CommentRepositoryAlgebra[F],
      articleValidation: ArticleValidationAlgebra[F],
      userValidation: UserValidationAlgebra[F]): CommentValidationInterpreter[F] =
    new CommentValidationInterpreter(commentRepo, articleValidation, userValidation)
}
