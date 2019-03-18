package io.github.niblius.erinye.infrastructure.endpoint
import cats.FlatMap
import cats.data.NonEmptyList
import cats.implicits._
import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.github.niblius.erinye.domain.ValidationError
import org.http4s.Response

/**
  * Used to build responses out of the set of errors
  */
case class ResponseBuilder[F[_]: FlatMap, A <: ValidationError](
    left: A => String => F[Response[F]],
    leftSequence: NonEmptyList[String] => F[Response[F]],
    right: Json => F[Response[F]]) {
  def build[B](result: F[Either[A, B]])(implicit entityEncoder: Encoder[B]): F[Response[F]] =
    result.flatMap {
      case Left(a) => left(a)(a.explanation)
      case Right(b) => right(b.asJson)
    }

  def buildList[B](result: F[Either[NonEmptyList[A], B]])(
      implicit entityEncoder: Encoder[B]): F[Response[F]] =
    result.flatMap {
      case Left(as) => leftSequence(as.map(_.explanation))
      case Right(b) => right(b.asJson)
    }

  def buildLeft[B](result: F[Either[A, B]])(customRight: B => F[Response[F]]): F[Response[F]] =
    result.flatMap {
      case Left(a) => left(a)(a.explanation)
      case Right(b) => customRight(b)
    }

  def buildLeftList[B](result: F[Either[NonEmptyList[A], B]])(
      customRight: B => F[Response[F]]): F[Response[F]] =
    result.flatMap {
      case Left(as) => leftSequence(as.map(_.explanation))
      case Right(b) => customRight(b)
    }
}
