package io.github.niblius.erinye.domain.notifications

import cats.data.EitherT
import cats.implicits._
import cats.effect.{Concurrent, Sync}
import io.github.niblius.erinye.domain.articles._
import org.http4s.websocket.WebSocketFrame
import io.circe.syntax._
import io.circe.Encoder
import fs2.{Pipe, Stream}
import fs2.concurrent.{SignallingRef, Topic}
import org.http4s.websocket.WebSocketFrame.Text

import scala.collection.concurrent.TrieMap

class NotificationService[F[_]: Concurrent](articleValidation: ArticleValidationAlgebra[F]) {

  val topics: TrieMap[Long, Topic[F, WebSocketFrame]] =
    TrieMap.empty[Long, Topic[F, WebSocketFrame]]

  def subscribe(articleId: Long, enqueue: Pipe[F, WebSocketFrame, Unit])(
      implicit F: Sync[F]): EitherT[F, NoSuchArticleError.type, SignallingRef[F, Boolean]] =
    for {
      _ <- articleValidation.exists(articleId).leftMap(_ => NoSuchArticleError)
      newTopic <- EitherT.right(Topic[F, WebSocketFrame](Text("")))
      topic <- EitherT.right(F.delay(topics.getOrElseUpdate(articleId, newTopic)))
      signal <- EitherT.right(SignallingRef(false))
      stream = topic.subscribe(128).interruptWhen(signal)
      _ <- EitherT.right(stream.through(enqueue).compile.drain)
    } yield signal

  def publish(articleId: Long, msg: String)(implicit F: Sync[F]): F[Unit] =
    F.delay(topics.get(articleId)).flatMap {
      case Some(topic) =>
        Stream
          .emit(msg)
          .map[WebSocketFrame](Text(_))
          .covary[F]
          .through(topic.publish)
          .compile
          .drain
      case None => ().pure[F]
    }

  def publish[N <: Notification](notification: N)(implicit encoder: Encoder[N]): F[Unit] =
    publish(notification.articleId, notification.asJson.toString())
}

object NotificationService {
  def apply[F[_]: Concurrent](
      articleValidation: ArticleValidationAlgebra[F]): NotificationService[F] =
    new NotificationService(articleValidation)
}
