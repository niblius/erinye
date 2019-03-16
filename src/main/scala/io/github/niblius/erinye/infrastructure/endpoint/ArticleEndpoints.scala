package io.github.niblius.erinye.infrastructure.endpoint

import cats.data.Validated.Valid
import cats.data._
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import io.github.niblius.erinye.domain.articles.{Article, ArticleNotFoundError, ArticleService}
import io.github.niblius.erinye.domain.notifications.{
  ArticleDeleted,
  ArticleUpdated,
  NotificationService
}
import io.github.niblius.erinye.infrastructure.endpoint.Pagination.{
  OptionalOffsetMatcher,
  OptionalPageSizeMatcher
}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, HttpRoutes}

import scala.language.higherKinds

class ArticleEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  /* Parses out tag query param, which could be multi-value */
  object TagMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("tags")

  implicit val articleDecoder: EntityDecoder[F, Article] = jsonOf[F, Article]

  private def createArticleEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "articles" =>
        for {
          article <- req.as[Article]
          saved <- articleService.create(article)
          resp <- Ok(saved.asJson)
        } yield resp
    }

  private def updateArticleEndpoint(
      articleService: ArticleService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ PUT -> Root / "articles" / LongVar(articleId) =>
        val action = for {
          article <- req.as[Article]
          updated = article.copy(id = Some(articleId))
          result <- articleService.update(article).value
          _ <- notificationService.publish(ArticleUpdated(updated))
        } yield result

        action.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(ArticleNotFoundError) => NotFound("The article was not found")
        }
    }

  private def getArticleEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" / LongVar(id) =>
        articleService.get(id).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(ArticleNotFoundError) => NotFound("The article was not found")
        }
    }

  private def deleteArticleEndpoint(
      articleService: ArticleService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case DELETE -> Root / "articles" / LongVar(id) =>
        val action = for {
          deleted <- articleService.delete(id)
          _ <- EitherT.liftF[F, ArticleNotFoundError.type, Unit](
            notificationService.publish(ArticleDeleted(deleted)))
        } yield ()

        action.value.flatMap {
          case Right(_) => Ok()
          case Left(ArticleNotFoundError) => NotFound("The article was not found")
        }
    }

  private def listArticlesEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" :? OptionalPageSizeMatcher(pageSize) :? OptionalOffsetMatcher(
            offset) =>
        for {
          retrieved <- articleService.list(pageSize.getOrElse(10), offset.getOrElse(0))
          resp <- Ok(retrieved.asJson)
        } yield resp
    }

  private def searchArticlesByTitle(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" / "searchByTitle" / title =>
        if (title.isBlank) for {
          retrieved <- articleService.searchByTitle(title)
          resp <- Ok(retrieved.asJson)
        } yield resp
        else
          Ok(List[Article]().asJson)
    }

  private def findArticlesByTagEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" / "findByTags" :? TagMatcher(Valid(Nil)) =>
        BadRequest("tag parameter not specified")

      case GET -> Root / "articles" / "findByTags" :? TagMatcher(Valid(tags)) =>
        for {
          retrieved <- articleService.findByTag(NonEmptyList.fromListUnsafe(tags))
          resp <- Ok(retrieved.asJson)
        } yield resp

    }

  def endpoints(
      articleService: ArticleService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    createArticleEndpoint(articleService) <+>
      getArticleEndpoint(articleService) <+>
      deleteArticleEndpoint(articleService, notificationService) <+>
      listArticlesEndpoint(articleService) <+>
      searchArticlesByTitle(articleService) <+>
      updateArticleEndpoint(articleService, notificationService) <+>
      findArticlesByTagEndpoint(articleService)
}

object ArticleEndpoints {
  def endpoints[F[_]: Effect](
      articleService: ArticleService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    new ArticleEndpoints[F].endpoints(articleService, notificationService)
}
