package io.github.niblius.erinye.domain.users
import io.github.niblius.erinye.domain.ValidationError

sealed trait UserValidationError extends Product with Serializable with ValidationError

case object UserNotFoundError extends UserValidationError {
  def explanation: String = "User not found"
}

case object UserAlreadyExistsError extends UserValidationError {
  def explanation: String = "User already exists"
}

case object UserAuthenticationFailedError extends UserValidationError {
  def explanation: String = "User authentication failed"
}

case object UserForbiddenError extends UserValidationError {
  def explanation: String = "Forbidden for this user"
}

case object InvalidUserNameError extends UserValidationError {
  def explanation: String = "Invalid username"
}

case object InvalidEmailError extends UserValidationError {
  def explanation: String = "Invalid email"
}

case object InvalidPasswordError extends UserValidationError {
  def explanation: String =
    """Invalid password. The password must be 8 characters long and must then contain characters from at least 3 of the following 4 rules:
      |1 Upper case
      |1 Lower case
      |1 Numbers
      |1 Non-alpha numeric
    """.stripMargin
}
