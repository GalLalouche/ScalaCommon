package common.rich.func.kats

import alleycats.Empty
import cats.{Monoid, Semigroup}
import cats.implicits.catsSyntaxSemigroup

object MoreMapInstances {
  implicit def mapMonoid[K, V: Semigroup]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def empty: Map[K, V] = Map()
    override def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
      // Not using updated for compatibility with 2.12.
      ys.foldLeft(xs) { case (acc, (k, v)) => acc.updated(k, acc.get(k).map(v.|+|).getOrElse(v)) }
  }
  implicit def mapEmpty[K, V]: Empty[Map[K, V]] = new Empty[Map[K, V]] {
    override def empty: Map[K, V] = Map.empty
  }
}
