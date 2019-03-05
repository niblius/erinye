package io.github.niblius.erinye.infrastructure.endpoint

import cats.implicits._
import cats.effect.{Async, ContextShift, Effect, IO}
import _root_.doobie.Transactor
import io.circe.config.parser
import io.github.niblius.erinye.config.{DatabaseConfig, ErinyeConfig}

import scala.concurrent.ExecutionContext

package object endpoint {
  def getTransactor[F[_] : Async : ContextShift](cfg : DatabaseConfig) : Transactor[F] =
    Transactor.fromDriverManager[F](
      cfg.driver,
      cfg.url,
      cfg.user,
      cfg.password
    )

  /*
   * Provide a transactor for testing once schema has been migrated.
   * TODO: Make separate test config
   */
  def initializedTransactor[F[_] : Effect : Async : ContextShift] : F[Transactor[F]] = for {
    conf <- parser.decodePathF[F, ErinyeConfig]("erinye")
    _ <- DatabaseConfig.initializeDb(conf.db)
  } yield getTransactor(conf.db)

  lazy val testEc = ExecutionContext.Implicits.global

  implicit lazy val testCs = IO.contextShift(testEc)

  lazy val testTransactor = initializedTransactor[IO].unsafeRunSync()
}
