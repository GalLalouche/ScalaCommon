package common.rich.collections

import common.rich.func.ToMoreFoldableOps

import scala.language.higherKinds
import scalaz.std.OptionInstances
import scalaz.syntax.ToSemigroupOps
import scalaz.{Plus, Semigroup}

object RichMap {
  implicit class richMap[K, V](private val $: Map[K, V]) extends AnyVal {
    /** Throws on duplicate keys. */
    def mapKeys[K2](f: K => K2): Map[K2, V] = {
      $.foldLeft(Map[K2, V]()) {case (map, (k, v)) =>
        val k2 = f(k)
        if (map contains k2)
          throw new UnsupportedOperationException(
            s"key <$k2> already exists in map but it is also transformed from key <$k>")
        map + (k2 -> v)
      }
    }
  }

  implicit class richSemigroupMap[K, V: Semigroup]($: Map[K, V])
      extends ToSemigroupOps with ToMoreFoldableOps with OptionInstances {
    def upsert(kv: (K, V)): Map[K, V] = upsert(kv._1, kv._2)
    def upsert(k: K, v: => V): Map[K, V] = $.updated(k, $.get(k).mapHeadOrElse(_ ⊹ v, v))
    def merge(other: Map[K, V]): Map[K, V] = other.foldLeft($)(_ upsert _)
    def mergeIntersecting(other: Map[K, V]): Map[K, V] =
      $.filterKeys(other.contains).map(e => (e._1, e._2 |+| other(e._1)))
  }
  implicit class richPlusMap[K, V[_] : Plus, A]($: Map[K, V[A]]) {
    private val asSemigroup = richSemigroupMap($)(Plus[V].semigroup)
    def merge(other: Map[K, V[A]]): Map[K, V[A]] = asSemigroup merge other
    def mergeIntersecting(other: Map[K, V[A]]): Map[K, V[A]] = asSemigroup mergeIntersecting other
  }
}
