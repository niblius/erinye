package io.github.niblius.erinye.infrastructure.endpoint

import cats.data.Validated.Valid
import cats.data._
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import io.github.niblius.erinye.domain.comments._
import io.github.niblius.erinye.domain.notifications.{CommentDeleted, NotificationService}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, HttpRoutes}

import scala.language.higherKinds

class CommentEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  /* Parses out tag query param, which could be multi-value */
  object TagMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("tags")

  // TODO: remove this?
  implicit val commentDecoder: EntityDecoder[F, Comment] = jsonOf[F, Comment]

  private def createCommentEndpoint(
      commentService: CommentService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "comments" =>
        for {
          comment <- req.as[Comment]
          saved <- commentService.create(comment)
          resp <- Ok(saved.asJson)
        } yield resp
    }

  private def updateCommentEndpoint(
      commentService: CommentService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ PUT -> Root / "comments" / LongVar(commentId) =>
        val action = for {
          comment <- req.as[Comment]
          updated = comment.copy(id = Some(commentId))
          result <- commentService.update(comment).value
        } yield result

        action.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(CommentNotFoundError) => NotFound("The comment was not found")
        }
    }

  private def getCommentEndpoint(commentService: CommentService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "comments" / LongVar(id) =>
        commentService.get(id).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(CommentNotFoundError) => NotFound("The comment was not found")
        }
    }

  private def deleteCommentEndpoint(
      commentService: CommentService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case DELETE -> Root / "comments" / LongVar(id) =>
        val action = for {
          deleted <- commentService.delete(id)
          _ <- EitherT.liftF[F, CommentNotFoundError.type, Unit](
            notificationService.publish(CommentDeleted(deleted)))
        } yield ()

        action.value.flatMap {
          case Right(_) => Ok()
          case Left(CommentNotFoundError) => NotFound("The comment was not found")
        }
    }

  private def listCommentsEndpoint(commentService: CommentService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "comments" / "list" / LongVar(articleId) =>
        for {
          retrieved <- commentService.list(articleId)
          resp <- Ok(retrieved.asJson)
        } yield resp
    }

  def endpoints(
      commentService: CommentService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    createCommentEndpoint(commentService, notificationService) <+>
      getCommentEndpoint(commentService) <+>
      deleteCommentEndpoint(commentService, notificationService) <+>
      listCommentsEndpoint(commentService) <+>
      updateCommentEndpoint(commentService, notificationService)
}

object CommentEndpoints {
  def endpoints[F[_]: Effect](
      commentService: CommentService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    new CommentEndpoints[F].endpoints(commentService, notificationService)
}
