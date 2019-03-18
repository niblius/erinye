package io.github.niblius.erinye.domain.articles

import java.time.Instant

import cats.effect.Sync
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import io.circe.generic.auto._

case class Article(
    title: String,
    description: String,
    content: String,
    tags: Set[String],
    userId: Long,
    dateCreated: Instant,
    id: Option[Long] = None
)

object Article {
  implicit def articleDecoder[F[_]: Sync]: EntityDecoder[F, Article] = jsonOf
}
