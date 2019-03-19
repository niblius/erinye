package io.github.niblius.erinye.domain.comments

import io.github.niblius.erinye.domain.ValidationError

sealed trait CommentValidationError extends Product with Serializable with ValidationError

case object CommentNotFoundError extends CommentValidationError {
  def explanation: String = "Comment not found"
}

case object InvalidCommentBody extends CommentValidationError {
  def explanation: String = "Comment body cannot be empty"
}

case object CommentArticleNotFound extends CommentValidationError {
  def explanation: String = "Article with such id not found"
}

case object InvalidAnonymousName extends CommentValidationError {
  def explanation: String = "Anonymous name cannot be empty"
}

case object CommentForbidden extends CommentValidationError {
  def explanation: String = "You are only allowed to update or delete own comments"
}
