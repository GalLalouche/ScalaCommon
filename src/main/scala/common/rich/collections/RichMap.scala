package common.rich.collections

import scala.language.higherKinds
import scalaz.{Plus, Semigroup}

object RichMap {
  implicit class richSemigroupMap[K, V: Semigroup]($: Map[K, V]) {
    def merge(other: Map[K, V]): Map[K, V] = {
      def merge(smallMap: Map[K, V], largeMap: Map[K, V]): Map[K, V] =
        smallMap.foldLeft(largeMap)((agg, e) =>
          agg.updated(e._1, agg.get(e._1).map(Semigroup[V].append(_, e._2)).getOrElse(e._2)))
      if ($.size > other.size) merge(other, $) else merge($, other)
    }
    def mergeIntersecting(other: Map[K, V]): Map[K, V] =
      $.filterKeys(other.contains).map(e => (e._1, Semigroup[V].append(e._2, other(e._1))))
  }
  implicit class richPlusMap[K, V[_] : Plus, A]($: Map[K, V[A]]) {
    private val asSemigroup = richSemigroupMap($)(Plus[V].semigroup)
    def merge(other: Map[K, V[A]]): Map[K, V[A]] = asSemigroup merge other
    def mergeIntersecting(other: Map[K, V[A]]) = asSemigroup mergeIntersecting other
  }
}
