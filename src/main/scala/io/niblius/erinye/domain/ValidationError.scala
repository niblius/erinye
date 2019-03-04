package io.niblius.erinye.domain

sealed trait ValidationError extends Product with Serializable
case object ArticleNotFoundError extends ValidationError