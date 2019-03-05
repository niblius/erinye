package io.github.niblius.erinye
package domain.authentication

import domain.users.User
import tsec.passwordhashers.PasswordHash

final case class LoginRequest(
                               userName: String,
                               password: String
                             )

final case class SignupRequest(
                                userName: String,
                                email: String,
                                password: String
                              ){
  def asUser[A](hashedPassword: PasswordHash[A]) : User = User(
    userName,
    email,
    hashedPassword.toString
  )
}
