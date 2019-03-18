package io.github.niblius.erinye.domain.articles

import cats._
import cats.data._
import cats.effect.Sync
import cats.implicits._
import io.github.niblius.erinye.domain.users.UserValidationAlgebra

class ArticleValidationInterpreter[F[_]: Sync](
    repository: ArticleRepositoryAlgebra[F],
    userValidation: UserValidationAlgebra[F])
    extends ArticleValidationAlgebra[F] {

  def exists(articleId: Long): EitherT[F, ArticleValidationError, Article] =
    EitherT(repository.get(articleId).map(_.toRight(ArticleNotFoundError)))

  def validateTitle(title: String): EitherT[F, ArticleValidationError, Unit] =
    EitherT.cond[F](!title.isBlank, (), BadArticleTitleError)

  def validateDesc(description: String): EitherT[F, ArticleValidationError, Unit] =
    EitherT.cond[F](!description.isBlank, (), BadArticleDescriptionError)

  def validateContent(content: String): EitherT[F, ArticleValidationError, Unit] =
    EitherT.cond[F](!content.isBlank, (), BadArticleContentError)

  def validateTags(tags: Set[String]): EitherT[F, ArticleValidationError, Unit] =
    EitherT.cond[F](!tags.exists((s: String) => s.isBlank), (), BadArticleTagsError)

  def validateAuthor(userId: Long): EitherT[F, ArticleValidationError, Unit] =
    userValidation.exists(userId).leftMap(_ => BadArticleAuthorError)

  def validate(article: Article): F[ValidatedNel[ArticleValidationError, Unit]] =
    (
      validateTitle(article.title).toValidatedNel,
      validateDesc(article.description).toValidatedNel,
      validateContent(article.content).toValidatedNel,
      validateTags(article.tags).toValidatedNel,
      validateAuthor(article.userId).toValidatedNel
    ).mapN((_, _, _, _, _) => ().validNel)
}

object ArticleValidationInterpreter {
  def apply[F[_]: Sync](
      repository: ArticleRepositoryAlgebra[F],
      userValidation: UserValidationAlgebra[F]) =
    new ArticleValidationInterpreter[F](repository, userValidation)
}
