package io.github.niblius.erinye.domain.users

case class User(
    userName: String,
    email: String,
    hash: String,
    isAdmin: Boolean = false,
    id: Option[Long] = None,
)
