package io.niblius.erinye.infrastructure.endpoint

import cats.data.Validated.Valid
import cats.data._
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import io.niblius.erinye.domain.ArticleNotFoundError
import io.niblius.erinye.domain.articles.{Article, ArticleService}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, HttpRoutes}

import scala.language.higherKinds

class ArticleEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  import Pagination._

  /* Parses out tag query param, which could be multi-value */
  object TagMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("tags")

  implicit val articleDecoder: EntityDecoder[F, Article] = jsonOf[F, Article]

  private def createArticleEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "articles" => for {
          article <- req.as[Article]
          saved <- articleService.create(article)
          resp <- Ok(saved.asJson)
        } yield resp
    }

  private def updateArticleEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ PUT -> Root / "articles" / LongVar(articleId) =>
        val action = for {
          article <- req.as[Article]
          updated = article.copy(id = Some(articleId))
          result <- articleService.update(article).value
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

  private def deleteArticleEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case DELETE -> Root / "articles" / LongVar(id) =>
        for {
          _ <- articleService.delete(id)
          resp <- Ok()
        } yield resp
    }

  private def listArticlesEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" :? OptionalPageSizeMatcher(pageSize) :? OptionalOffsetMatcher(offset) =>
        for {
          retrieved <- articleService.list(pageSize.getOrElse(10), offset.getOrElse(0))
          resp <- Ok(retrieved.asJson)
        } yield resp
    }

  private def searchArticlesByTitle(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ GET -> Root / "articles" / "searchByTitle" / title =>
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

  def endpoints(articleService: ArticleService[F]): HttpRoutes[F] =
    createArticleEndpoint(articleService) <+>
      getArticleEndpoint(articleService) <+>
      deleteArticleEndpoint(articleService) <+>
      listArticlesEndpoint(articleService) <+>
      searchArticlesByTitle(articleService) <+>
      updateArticleEndpoint(articleService) <+>
      findArticlesByTagEndpoint(articleService)
}

object ArticleEndpoints {
  def endpoints[F[_]: Effect](articleService: ArticleService[F]): HttpRoutes[F] =
    new ArticleEndpoints[F].endpoints(articleService)
}