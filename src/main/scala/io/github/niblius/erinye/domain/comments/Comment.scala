package io.github.niblius.erinye.domain.comments
import java.time.Instant

import cats.effect.Sync
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import io.circe.generic.auto._

case class Comment(
    id: Option[Long],
    userId: Option[Long],
    articleId: Long,
    body: String,
    anonymName: Option[String],
    dateCreated: Instant,
    dateEdited: Option[Instant])

object Comment {
  implicit def commentDecoder[F[_]: Sync]: EntityDecoder[F, Comment] = jsonOf
}
