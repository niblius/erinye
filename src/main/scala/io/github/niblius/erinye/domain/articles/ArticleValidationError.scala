package io.github.niblius.erinye.domain.articles
import io.github.niblius.erinye.domain.ValidationError

sealed trait ArticleValidationError extends Product with Serializable with ValidationError

case object ArticleNotFoundError extends ArticleValidationError {
  def explanation: String = "Article not found"
}

case object BadArticleTitleError extends ArticleValidationError {
  def explanation: String = "Article title should not be blank"
}

case object BadArticleDescriptionError extends ArticleValidationError {
  def explanation: String = "Article description should not be blank"
}

case object BadArticleContentError extends ArticleValidationError {
  def explanation: String = "Article content should not be blank"
}

case object BadArticleTagsError extends ArticleValidationError {
  def explanation: String = "Article tag should not be empty string"
}

case object BadArticleAuthorError extends ArticleValidationError {
  def explanation: String = "Article should have correct and non-empty author id"
}
