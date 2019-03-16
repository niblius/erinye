package io.github.niblius.erinye.domain.comments

import cats._
import cats.implicits._
import cats.data._

class CommentService[F[_]: Functor](repository: CommentRepositoryAlgebra[F]) {

  def create(comment: Comment): F[Comment] =
    repository.create(comment)

  def update(comment: Comment): EitherT[F, CommentNotFoundError.type, Comment] =
    EitherT.fromOptionF(repository.update(comment), CommentNotFoundError)

  def get(id: Long): EitherT[F, CommentNotFoundError.type, Comment] =
    EitherT.fromOptionF(repository.get(id), CommentNotFoundError)

  def delete(id: Long): EitherT[F, CommentNotFoundError.type, Comment] =
    EitherT.fromOptionF(repository.delete(id), CommentNotFoundError)

  def list(articleId: Long): F[List[Comment]] =
    repository.list(articleId)
}

object CommentService {
  def apply[F[_]: Monad](repository: CommentRepositoryAlgebra[F]) =
    new CommentService[F](repository)
}
