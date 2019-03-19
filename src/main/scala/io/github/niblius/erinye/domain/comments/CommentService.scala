package io.github.niblius.erinye.domain.comments

import cats._
import cats.implicits._
import cats.data._

class CommentService[F[_]: Monad](
    repository: CommentRepositoryAlgebra[F],
    validation: CommentValidationAlgebra[F]) {

  private def createWithExternalValidation(
      comment: Comment,
      validate: F[ValidatedNel[CommentValidationError, Unit]])
    : EitherT[F, NonEmptyList[CommentValidationError], Comment] =
    for {
      _ <- EitherT(validate.map(_.toEither))
      saved <- EitherT.right(repository.create(comment))
    } yield saved

  def create(comment: Comment): EitherT[F, NonEmptyList[CommentValidationError], Comment] =
    createWithExternalValidation(comment, validation.validateComment(comment))

  def createAnonymous(comment: Comment): EitherT[F, NonEmptyList[CommentValidationError], Comment] =
    createWithExternalValidation(comment, validation.validateAnonymousComment(comment))

  def update(comment: Comment): EitherT[F, NonEmptyList[CommentValidationError], Comment] =
    for {
      _ <- EitherT(validation.validateComment(comment).map(_.toEither))
      saved <- EitherT.fromOptionF[F, NonEmptyList[CommentValidationError], Comment](
        repository.update(comment),
        NonEmptyList.one(CommentNotFoundError))
    } yield saved

  def get(id: Long): EitherT[F, CommentNotFoundError.type, Comment] =
    EitherT.fromOptionF(repository.get(id), CommentNotFoundError)

  def delete(id: Long): EitherT[F, CommentNotFoundError.type, Comment] =
    EitherT.fromOptionF(repository.delete(id), CommentNotFoundError)

  def list(articleId: Long): F[List[Comment]] =
    repository.list(articleId)
}

object CommentService {
  def apply[F[_]: Monad](
      repository: CommentRepositoryAlgebra[F],
      validation: CommentValidationAlgebra[F]) =
    new CommentService[F](repository, validation)
}
