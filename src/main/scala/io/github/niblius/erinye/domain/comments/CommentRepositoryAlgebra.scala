package io.github.niblius.erinye.domain.comments

trait CommentRepositoryAlgebra[F[_]] {
  def create(comment: Comment): F[Comment]

  def update(comment: Comment): F[Option[Comment]]

  def get(commentId: Long): F[Option[Comment]]

  def delete(commentId: Long): F[Option[Comment]]

  def list(articleId: Long): F[List[Comment]]
}
