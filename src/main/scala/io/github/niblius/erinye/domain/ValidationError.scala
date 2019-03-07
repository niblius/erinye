package io.github.niblius.erinye.domain

import io.github.niblius.erinye.domain.users.User

sealed trait ValidationError extends Product with Serializable
case object ArticleNotFoundError extends ValidationError
case object UserNotFoundError extends ValidationError
case class UserAlreadyExistsError(user: User) extends ValidationError
case class UserAuthenticationFailedError(userName: String) extends ValidationError
case object ForbiddenError extends ValidationError
