package io.github.niblius.erinye.infrastructure.endpoint

import cats.data._
import cats.effect._
import cats.implicits._
import io.github.niblius.erinye.domain.notifications._
import io.circe.generic.auto._
import io.circe.parser._
import org.http4s.dsl.Http4sDsl
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.http4s.HttpRoutes
import fs2.concurrent.{Queue, SignallingRef}
import fs2.{Pipe, Stream}
import io.circe.Decoder
import org.http4s.server.websocket.WebSocketBuilder

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds

sealed trait Command
final case class SubscribeCmd(subscribeToId: Long) extends Command
final case class UnsubscribeCmd(unsubscribeFromId: Long) extends Command

class WSEndpoints[F[_]: Concurrent] extends Http4sDsl[F] {
  implicit val cmdDecoder: Decoder[Command] =
    Decoder[SubscribeCmd].map[Command](identity).or(Decoder[UnsubscribeCmd].map[Command](identity))

  private def processCommand(
      service: NotificationService[F],
      enqueue: Pipe[F, WebSocketFrame, Unit],
      subscribed: TrieMap[Long, SignallingRef[F, Boolean]])(
      implicit F: Sync[F]): Pipe[F, WebSocketFrame, Unit] =
    _.collect {
      case Text(str, _) => decode[Command](str).leftMap[CommandError](_ => BadCommandError)
    }.evalMap {
        case Right(SubscribeCmd(subscribeTo)) =>
          (for {
            _ <- EitherT(
              F.delay(
                subscribed
                  .get(subscribeTo)
                  .toLeft(())
                  .leftMap[CommandError](_ => AlreadySubscribedError)))
            signal <- service.subscribe(subscribeTo, enqueue)
            _ <- EitherT.right[CommandError](F.delay(subscribed.put(subscribeTo, signal)))
          } yield ()).value
        case Right(UnsubscribeCmd(unsubscribeFrom)) =>
          (for {
            signal <- EitherT.fromOptionF(
              F.delay(subscribed.remove(unsubscribeFrom)),
              WasNotSubscribedError)
            _ <- EitherT.right[CommandError](F.delay(signal.set(true)))
          } yield ()).value
        case error => error.map(_ => ()).pure[F]
      }
      .evalMap {
        case Left(error: CommandError) =>
          Stream
            .emit(s""" { "error": "${error.errorMessage}" }""")
            .map[WebSocketFrame](Text(_))
            .covary[F]
            .through(enqueue)
            .compile
            .drain
        case _ => ().pure[F]
      }
      .drain

  private def onClose(subscribed: TrieMap[Long, SignallingRef[F, Boolean]]): F[Unit] =
    Sync[F].defer(subscribed.foldLeft(().pure[F])((eff, t) => eff.flatMap(_ => t._2.set(true))))

  private def notificationEndpoint(notifyService: NotificationService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "ws" =>
        for {
          queue <- Queue.unbounded[F, WebSocketFrame]
          toClient = queue.dequeue
          subscribed = TrieMap.empty[Long, SignallingRef[F, Boolean]]
          fromClient = processCommand(notifyService, queue.enqueue, subscribed)
          ws <- WebSocketBuilder[F].build(
            send = toClient,
            receive = fromClient,
            onClose = onClose(subscribed))
        } yield ws
    }

  def endpoints(notificationService: NotificationService[F]): HttpRoutes[F] =
    notificationEndpoint(notificationService)
}

object WSEndpoints {
  def endpoints[F[_]: Concurrent](notificationService: NotificationService[F]): HttpRoutes[F] =
    new WSEndpoints[F].endpoints(notificationService)
}
