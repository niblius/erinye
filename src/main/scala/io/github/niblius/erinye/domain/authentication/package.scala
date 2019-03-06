package io.github.niblius.erinye.domain
import cats.effect.Sync
import io.github.niblius.erinye.domain.users.{Role, User}
import tsec.authentication.AugmentedJWT
import tsec.authorization.BasicRBAC
import tsec.mac.jca.HMACSHA256

package object authentication {
  def AdminRequired[F[_]: Sync]: BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Int]] =
    BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Int]](Role.Administrator)
  def AuthenticatedRequired[F[_]: Sync]: BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Int]] =
    BasicRBAC[F, Role, User, AugmentedJWT[HMACSHA256, Int]](Role.Administrator, Role.PlainUser)
}
