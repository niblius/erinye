package io.github.niblius.erinye.domain.comments
import java.time.Instant

case class Comment(
    id: Option[Long],
    userId: Option[Long],
    articleId: Long,
    body: String,
    anonymName: Option[String],
    dateCreated: Instant,
    dateEdited: Instant)
