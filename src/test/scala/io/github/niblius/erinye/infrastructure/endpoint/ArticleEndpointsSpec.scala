package io.github.niblius.erinye.infrastructure.endpoint

import io.github.niblius.erinye.infrastructure.ErinyeArbitraries
import io.github.niblius.erinye.domain.articles._
import io.github.niblius.erinye.infrastructure.doobie.DoobieArticleRepositoryInterpreter
import cats.effect._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl._
import org.http4s.circe._
import org.http4s.client.dsl.Http4sClientDsl
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import endpoint._


class ArticleEndpointsSpec
  extends FunSuite
    with Matchers
    with PropertyChecks
    with ErinyeArbitraries
    with Http4sDsl[IO]
    with Http4sClientDsl[IO]{

  implicit val articleEnc : EntityEncoder[IO, Article] = jsonEncoderOf
  implicit val articleDec : EntityDecoder[IO, Article] = jsonOf

  val articleRepo = DoobieArticleRepositoryInterpreter[IO](testTransactor)
  val articleValidation = ArticleValidationInterpreter[IO](articleRepo)
  val articleService = ArticleService[IO](articleRepo, articleValidation)
  val articleHttpService = ArticleEndpoints.endpoints[IO](articleService).orNotFound

  test("create article") {
    forAll { article: Article =>
      (for {
        request <- POST(article, Uri.uri("/articles"))
        response <- articleHttpService.run(request)
      } yield {
        response.status shouldEqual Ok
      }).unsafeRunSync
    }
  }

  test("update article") {
    forAll { article: Article =>
      (for {
        createRequest <- POST(article, Uri.uri("/articles"))
        createResponse <- articleHttpService.run(createRequest)
        createdArticle <- createResponse.as[Article]
        articleToUpdate = createdArticle.copy(title = createdArticle.title.reverse)
        updateRequest <- PUT(articleToUpdate, Uri.unsafeFromString(s"/articles/${articleToUpdate.id.get}"))
        updateResponse <- articleHttpService.run(updateRequest)
        updatedArticle <- updateResponse.as[Article]
      } yield {
        updatedArticle.title shouldEqual article.title.reverse
      }).unsafeRunSync
    }
  }

}