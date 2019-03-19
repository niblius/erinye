package io.github.niblius.erinye.infrastructure.endpoint

import java.time.Instant

import cats.data._
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import io.github.niblius.erinye.domain.authentication.AuthenticatedRequired
import io.github.niblius.erinye.domain.comments._
import io.github.niblius.erinye.domain.notifications._
import io.github.niblius.erinye.domain.users.{Role, User}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpRoutes
import tsec.authentication._
import tsec.mac.jca.HMACSHA256
import io.github.niblius.erinye.domain.authentication._

import scala.language.higherKinds

class CommentEndpoints[F[_]: Effect] extends Http4sDsl[F] {

  private val responseBuilder = ResponseBuilder[F, CommentValidationError](
    {
      case CommentNotFoundError => NotFound(_)
      case CommentArticleNotFound => NotFound(_)
      case _ => BadRequest(_)
    },
    jsons => BadRequest(jsons),
    json => Ok(json)
  )

  private def createCommentEndpoint(
      commentService: CommentService[F],
      notificationService: NotificationService[F])
    : TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AuthenticatedRequired) {
      case secured @ POST -> Root / "comments" asAuthed user =>
        val req = secured.request
        val action: EitherT[F, NonEmptyList[CommentValidationError], Comment] = for {
          comment <- EitherT.liftF(req.as[Comment])
          saved <- commentService
            .create(
              comment.copy(
                anonymName = None,
                userId = user.id,
                dateCreated = Instant.now(),
                dateEdited = None))
          _ <- EitherT.liftF(notificationService.publish(CommentCreated(saved)))
        } yield saved
        responseBuilder.buildList(action.value)
    }

  private def createAnonymousCommentEndpoint(
      commentService: CommentService[F],
      notificationService: NotificationService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "comments" / "anonymous" =>
        val action: EitherT[F, NonEmptyList[CommentValidationError], Comment] = for {
          comment <- EitherT.liftF(req.as[Comment])
          saved <- commentService
            .createAnonymous(
              comment.copy(userId = None, dateCreated = Instant.now(), dateEdited = None))
          _ <- EitherT.liftF(notificationService.publish(CommentCreated(saved)))
        } yield saved
        responseBuilder.buildList(action.value)
    }

  private def updateCommentEndpoint(
      commentService: CommentService[F],
      notificationService: NotificationService[F])
    : TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AuthenticatedRequired) {
      case secured @ PUT -> Root / "comments" / LongVar(commentId) asAuthed user =>
        val req = secured.request
        val action: EitherT[F, NonEmptyList[CommentValidationError], Comment] = for {
          comment <- EitherT.liftF(req.as[Comment])
          orig <- commentService.get(commentId).leftMap(NonEmptyList.one)
          updated <- {
            if (orig.userId.get == user.id.get || user.role == Role.Administrator)
              EitherT.rightT[F, NonEmptyList[CommentValidationError]](
                comment.copy(
                  id = orig.id,
                  userId = orig.userId,
                  articleId = orig.articleId,
                  anonymName = None,
                  dateCreated = orig.dateCreated,
                  dateEdited = Some(Instant.now())))
            else EitherT.leftT[F, Comment](NonEmptyList.one(CommentForbidden))
          }
          saved <- commentService.update(updated)
          _ <- EitherT.liftF(notificationService.publish(CommentEdited(saved)))
        } yield saved

        responseBuilder.buildList(action.value)
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
      notificationService: NotificationService[F])
    : TSecAuthService[User, AugmentedJWT[HMACSHA256, Long], F] =
    TSecAuthService.withAuthorization(AuthenticatedRequired) {
      case DELETE -> Root / "comments" / LongVar(id) asAuthed user =>
        val action: EitherT[F, CommentValidationError, Unit] = for {
          comment <- commentService.get(id)
          deleted <- {
            if (comment.userId.get == user.id.get || user.role == Role.Administrator)
              commentService.delete(id)
            else
              EitherT.leftT[F, Comment](CommentForbidden)
          }
          _ <- EitherT.liftF(notificationService.publish(CommentDeleted(deleted)))
        } yield ()

        responseBuilder.build(action.value)
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
      notificationService: NotificationService[F],
      authService: AuthService[F]): HttpRoutes[F] =
    createAnonymousCommentEndpoint(commentService, notificationService) <+>
      getCommentEndpoint(commentService) <+>
      listCommentsEndpoint(commentService) <+>
      authService.Auth.liftWithFallthrough(
        updateCommentEndpoint(commentService, notificationService) <+>
          deleteCommentEndpoint(commentService, notificationService) <+>
          createCommentEndpoint(commentService, notificationService)
      )
}

object CommentEndpoints {
  def endpoints[F[_]: Effect](
      commentService: CommentService[F],
      notificationService: NotificationService[F],
      authService: AuthService[F]): HttpRoutes[F] =
    new CommentEndpoints[F].endpoints(commentService, notificationService, authService)
}
