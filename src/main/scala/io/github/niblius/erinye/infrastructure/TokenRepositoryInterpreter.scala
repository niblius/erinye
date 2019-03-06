package io.github.niblius.erinye.infrastructure

import cats.data.OptionT
import cats.effect.Sync
import tsec.authentication.BackingStore

import scala.collection.concurrent.TrieMap

// TODO: Use redis, come on...
class TokenRepositoryInterpreter[F[_], I, V](getId: V => I)(implicit F: Sync[F])
    extends BackingStore[F, I, V] {
  private val storageMap = TrieMap.empty[I, V]

  def put(elem: V): F[V] = {
    val map = storageMap.put(getId(elem), elem)
    if (map.isEmpty)
      F.pure(elem)
    else
      F.raiseError(new IllegalArgumentException)
  }

  def get(id: I): OptionT[F, V] =
    OptionT.fromOption[F](storageMap.get(id))

  def update(v: V): F[V] = {
    storageMap.update(getId(v), v)
    F.pure(v)
  }

  def delete(id: I): F[Unit] =
    storageMap.remove(id) match {
      case Some(_) => F.unit
      case None => F.raiseError(new IllegalArgumentException)
    }
}

object TokenRepositoryInterpreter {
  def apply[F[_], I, V](getId: V => I)(implicit F: Sync[F]): TokenRepositoryInterpreter[F, I, V] =
    new TokenRepositoryInterpreter[F, I, V](getId)
}
