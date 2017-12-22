package common.rich.collections

import scala.language.higherKinds
import scalaz.syntax.ToSemigroupOps
import scalaz.{Plus, Semigroup}

object RichMap {
  implicit class richSemigroupMap[K, V: Semigroup]($: Map[K, V]) extends ToSemigroupOps {
    def merge(other: Map[K, V]): Map[K, V] = {
      def aux(smallMap: Map[K, V], largeMap: Map[K, V]): Map[K, V] = smallMap.foldLeft(largeMap)(_ upsert _)
      if ($.size > other.size) aux(other, $) else aux($, other)
    }
    def upsert(kv: (K, V)): Map[K, V] = upsert(kv._1, kv._2)
    def upsert(k: K, v: => V): Map[K, V] = $.updated(k, $.get(k).fold(v)(_ |+| v))
    def mergeIntersecting(other: Map[K, V]): Map[K, V] =
      $.filterKeys(other.contains).map(e => (e._1, e._2 |+| other(e._1)))
  }
  implicit class richPlusMap[K, V[_] : Plus, A]($: Map[K, V[A]]) {
    private val asSemigroup = richSemigroupMap($)(Plus[V].semigroup)
    def merge(other: Map[K, V[A]]): Map[K, V[A]] = asSemigroup merge other
    def mergeIntersecting(other: Map[K, V[A]]) = asSemigroup mergeIntersecting other
  }
}
