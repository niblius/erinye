package io.github.niblius.erinye.domain.notifications

sealed trait CommandError {
  def errorMessage: String
}

case object NoSuchArticleError extends CommandError {
  def errorMessage: String = "Cannot subscribe to non-existing article"
}

case object AlreadySubscribedError extends CommandError {
  def errorMessage: String = "Subscription already exists"
}

case object WasNotSubscribedError extends CommandError {
  def errorMessage: String = "Cannot unsubscribe from "
}
case object BadCommandError extends CommandError {
  def errorMessage: String = "Bad request"
}
