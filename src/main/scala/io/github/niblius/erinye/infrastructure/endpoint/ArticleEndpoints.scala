package io.github.niblius.erinye.infrastructure.endpoint

import java.time.Instant

import cats.data.Validated.Valid
import cats.data._
import cats.effect.{Effect, Sync}
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import io.github.niblius.erinye.domain.articles._
import io.github.niblius.erinye.domain.authentication._
import io.github.niblius.erinye.domain.notifications.{
  ArticleDeleted,
  ArticleUpdated,
  NotificationService
}
import io.github.niblius.erinye.domain.users.User
import io.github.niblius.erinye.infrastructure.endpoint.Pagination.{
  OptionalOffsetMatcher,
  OptionalPageSizeMatcher
}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpRoutes
import tsec.authentication.{AugmentedJWT, TSecAuthService, asAuthed}
import tsec.mac.jca.HMACSHA256

import scala.language.higherKinds

class ArticleEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  /* Parses out tag query param, which could be multi-value */
  object TagMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("tags")

  type ArticleValidationErrors = NonEmptyList[ArticleValidationError]

  private val responseBuilder = ResponseBuilder[F, ArticleValidationError](
    {
      case ArticleNotFoundError => NotFound(_)
      case _ => BadRequest(_)
    },
    errors => BadRequest(errors.asJson),
    json => Ok(json)
  )

  private def createArticleEndpoint(
      articleService: ArticleService[F]): TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AdminRequired) {
      case secured @ POST -> Root / "articles" asAuthed user =>
        val req = secured.request
        val action: EitherT[F, ArticleValidationErrors, Article] = for {
          articleIn <- EitherT.right(req.as[Article])
          currentTime <- EitherT.right(Sync[F].delay(Instant.now()))
          article = articleIn.copy(dateCreated = currentTime, userId = user.id.get)
          saved <- articleService.create(article)
        } yield saved
        responseBuilder.buildList(action.value)
    }

  private def updateArticleEndpoint(
      articleService: ArticleService[F],
      notificationService: NotificationService[F])
    : TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AdminRequired) {
      case secured @ PUT -> Root / "articles" / LongVar(articleId) asAuthed user =>
        val req = secured.request
        val action: F[Either[ArticleValidationErrors, Article]] = for {
          article <- req.as[Article]
          updated = article.copy(
            id = Some(articleId),
            dateCreated = article.dateCreated,
            userId = article.userId)
          result <- articleService.update(article).value
          _ <- notificationService.publish(ArticleUpdated(updated))
        } yield result

        responseBuilder.buildList(action)
    }

  private def getArticleEndpoint(articleService: ArticleService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "articles" / LongVar(id) =>
        responseBuilder.build(articleService.get(id).leftWiden[ArticleValidationError].value)
    }

  private def deleteArticleEndpoint(
      articleService: ArticleService[F],
      notificationService: NotificationService[F])
    : TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AdminRequired) {
      case DELETE -> Root / "articles" / LongVar(id) asAuthed user =>
        val action = for {
          deleted <- articleService.delete(id).leftWiden[ArticleValidationError]
          _ <- EitherT.liftF[F, ArticleValidationError, Unit](
            notificationService.publish(ArticleDeleted(deleted)))
        } yield ()

        responseBuilder.build(action.value)
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
      notificationService: NotificationService[F],
      authService: AuthService[F]): HttpRoutes[F] =
    getArticleEndpoint(articleService) <+>
      listArticlesEndpoint(articleService) <+>
      searchArticlesByTitle(articleService) <+>
      findArticlesByTagEndpoint(articleService) <+>
      authService.Auth.liftWithFallthrough(
        createArticleEndpoint(articleService) <+>
          deleteArticleEndpoint(articleService, notificationService) <+>
          updateArticleEndpoint(articleService, notificationService)
      )
}

object ArticleEndpoints {
  def endpoints[F[_]: Effect](
      articleService: ArticleService[F],
      notificationService: NotificationService[F],
      authService: AuthService[F]): HttpRoutes[F] =
    new ArticleEndpoints[F].endpoints(articleService, notificationService, authService)
}
