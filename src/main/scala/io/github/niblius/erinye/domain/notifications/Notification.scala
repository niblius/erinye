package io.github.niblius.erinye.domain.notifications
import io.github.niblius.erinye.domain.articles.Article
import io.github.niblius.erinye.domain.comments.Comment

sealed trait Notification {
  def articleId: Long
}

final case class ArticleUpdated(articleUpdated: Article) extends Notification {
  def articleId: Long = articleUpdated.id.get
}

final case class ArticleEdited(articleEdited: Article) extends Notification {
  def articleId: Long = articleEdited.id.get
}

final case class ArticleDeleted(articleDeleted: Article) extends Notification {
  def articleId: Long = articleDeleted.id.get
}

final case class CommentCreated(commentCreated: Comment) extends Notification {
  def articleId: Long = commentCreated.articleId
}

final case class CommentEdited(commentEdited: Comment) extends Notification {
  def articleId: Long = commentEdited.articleId
}

final case class CommentDeleted(commentDeleted: Comment) extends Notification {
  def articleId: Long = commentDeleted.articleId
}
