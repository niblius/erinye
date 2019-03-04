package io.niblius.erinye

import config._
import cats.effect._
import cats.implicits._
import org.http4s.server.{Router, Server => H4Server}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.ExecutionContext.Implicits.global
import io.circe.config.parser
import io.niblius.erinye.domain.articles.{ArticleService, ArticleValidationInterpreter}
import io.niblius.erinye.infrastructure.doobie.DoobieArticleRepositoryInterpreter
import io.niblius.erinye.infrastructure.endpoint.ArticleEndpoints


object Server extends IOApp {
  def createServer[F[_] : ContextShift : ConcurrentEffect : Timer] //: Resource[F, H4Server[F]] =
    =
    for {
      conf                <- Resource.liftF(parser.decodePathF[F, ErinyeConfig]("erinye"))
      xa                  <- DatabaseConfig.dbTransactor(conf.db, global, global)
      articleRepo         =  DoobieArticleRepositoryInterpreter[F](xa)
      articleValidation   =  ArticleValidationInterpreter[F](articleRepo)
      articleService      =  ArticleService[F](articleRepo, articleValidation)
      services            =  ArticleEndpoints.endpoints[F](articleService)
      httpApp = Router("/" -> services).orNotFound
      _ <- Resource.liftF(DatabaseConfig.initializeDb(conf.db))
      server <-
        BlazeServerBuilder[F]
          .bindHttp(conf.server.port, conf.server.host)
          .withHttpApp(httpApp)
          .resource
    } yield server

  def run(args : List[String]) : IO[ExitCode] = createServer.use(_ => IO.never).as(ExitCode.Success)
}
