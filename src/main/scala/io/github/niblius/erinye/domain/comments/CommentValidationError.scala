package io.github.niblius.erinye.domain.comments

sealed trait CommentValidationError
case object CommentNotFoundError extends CommentValidationError
