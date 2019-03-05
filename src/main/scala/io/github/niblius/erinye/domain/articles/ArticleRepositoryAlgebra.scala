package io.github.niblius.erinye.domain.articles

import cats.data.NonEmptyList

trait ArticleRepositoryAlgebra[F[_]] {

  def create(article: Article): F[Article]

  def update(pet: Article) : F[Option[Article]]

  def get(id: Long): F[Option[Article]]

  def delete(id: Long): F[Option[Article]]

  def list(pageSize: Int, offset: Int): F[List[Article]]

  def findByTag(tags: NonEmptyList[String]): F[List[Article]]

  def searchByTitle(title: String): F[List[Article]]
}
