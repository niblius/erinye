package io.github.niblius.erinye.domain.articles

import java.time.Instant

case class Article(
    title: String,
    description: String,
    content: String,
    tags: Set[String],
    dateCreated: Instant,
    id: Option[Long] = None
)
