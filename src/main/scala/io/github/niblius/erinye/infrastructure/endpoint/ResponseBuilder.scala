package io.github.niblius.erinye.infrastructure.endpoint
import cats.FlatMap
import cats.implicits._
import org.http4s.Response

/**
  * Used to build responses out of the set of errors
  */
case class ResponseBuilder[F[_]: FlatMap, A](left: A => F[Response[F]]) {
  def buildResponse[B](result: F[Either[A, B]])(right: B => F[Response[F]]): F[Response[F]] =
    result.flatMap {
      case Left(a) => left(a)
      case Right(b) => right(b)
    }
}
