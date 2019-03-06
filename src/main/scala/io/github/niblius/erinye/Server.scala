package io.github.niblius.erinye

import cats.Id
import io.circe.config.parser
import io.github.niblius.erinye.config.{DatabaseConfig, ErinyeConfig}
import io.github.niblius.erinye.domain.articles.{ArticleService, ArticleValidationInterpreter}
import io.github.niblius.erinye.infrastructure.doobie.{
  DoobieArticleRepositoryInterpreter,
  DoobieUserRepositoryInterpreter
}
import io.github.niblius.erinye.infrastructure.endpoint.{ArticleEndpoints, UserEndpoints}
import cats.effect._
import cats.implicits._
import io.github.niblius.erinye.domain.authentication.AuthService
import io.github.niblius.erinye.domain.users.{UserService, UserValidationInterpreter}
import io.github.niblius.erinye.infrastructure.TokenRepositoryInterpreter
import org.http4s.server.{Router, Server => H4Server}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._
import tsec.authentication.AugmentedJWT
import tsec.common.SecureRandomId
import tsec.mac.jca.{HMACSHA256}
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.ExecutionContext.Implicits.global

object Server extends IOApp {
  def createServer[F[_]: ContextShift: ConcurrentEffect: Timer]: Resource[F, H4Server[F]] =
    for {
      conf <- Resource.liftF(parser.decodePathF[F, ErinyeConfig]("erinye"))
      xa <- DatabaseConfig.dbTransactor(conf.db, global, global)
      articleRepo = DoobieArticleRepositoryInterpreter[F](xa)
      userRepo = DoobieUserRepositoryInterpreter[F](xa)
      articleValidation = ArticleValidationInterpreter[F](articleRepo)
      userValidation = UserValidationInterpreter[F](userRepo)
      articleService = ArticleService[F](articleRepo, articleValidation)
      userService = UserService[F](userRepo, userValidation)
      cryptService = BCrypt.syncPasswordHasher[F]
      tokenStore = TokenRepositoryInterpreter[F, SecureRandomId, AugmentedJWT[HMACSHA256, Long]](
        s => SecureRandomId.coerce(s.id))
      signingKey = HMACSHA256.generateKey[Id] // get from conf
      authService = AuthService[F](userService, tokenStore, signingKey, cryptService)
      services = ArticleEndpoints.endpoints[F](articleService) <+>
        UserEndpoints.endpoints[F](userService, cryptService, authService)
      httpApp = Router("/" -> services).orNotFound
      _ <- Resource.liftF(DatabaseConfig.initializeDb(conf.db))
      server <- BlazeServerBuilder[F]
        .bindHttp(conf.server.port, conf.server.host)
        .withHttpApp(httpApp)
        .resource
    } yield server

  def run(args: List[String]): IO[ExitCode] = createServer.use(_ => IO.never).as(ExitCode.Success)
}
