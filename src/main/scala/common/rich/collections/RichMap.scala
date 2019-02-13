package common.rich.collections

import java.util

import common.rich.func.ToMoreFoldableOps
import common.rich.primitives.RichBoolean._
import common.rich.RichT._

import scala.language.higherKinds

import scalaz.{Foldable, Plus, Semigroup}
import scalaz.std.OptionInstances
import scalaz.syntax.{ToFoldableOps, ToSemigroupOps}

object RichMap {
  implicit class richJavaMap[K, V](private val $: util.Map[K, V]) {
    /** *Not* inherently thread-safe. Use ConcurrentHashMap if you need one. */
    def getOrPutIfAbsent(k: K, v: => V): V = {
      if ($.containsKey(k).isFalse) // Not using putIfAbsent since its strict in its argument
        $.put(k, v)
      $.get(k)
    }

    def getOpt(k: K): Option[V] = $.get(k).opt
  }

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
    def merge(other: Map[K, V]): Map[K, V] = other.foldLeft($)(_ upsert _)
    def upsert[V2 >: V : Semigroup](kv: (K, V2)): Map[K, V2] = upsert(kv._1, kv._2)
    def upsert[V2 >: V : Semigroup](k: K, v: => V2): Map[K, V2] =
      $.updated(k, $.get(k).asInstanceOf[Option[V2]].mapHeadOrElse(_ ⊹ v, v))
    def mergeIntersecting(other: Map[K, V]): Map[K, V] =
      $.filterKeys(other.contains).map(e => (e._1, e._2 ⊹ other(e._1)))
  }
  implicit class richPlusMap[K, V[_] : Plus, A]($: Map[K, V[A]]) {
    private val asSemigroup = richSemigroupMap($)(Plus[V].semigroup)
    def merge(other: Map[K, V[A]]): Map[K, V[A]] = asSemigroup merge other
    def mergeIntersecting(other: Map[K, V[A]]): Map[K, V[A]] = asSemigroup mergeIntersecting other
  }
  implicit class richFoldableMap[K, V[_] : Foldable, A]($: Map[K, V[A]]) extends ToFoldableOps {
    def flattenValues: Seq[(K, A)] = $.toSeq.flatMap(e => e._2.toList.map(e._1.->))
    /** Complexity: O(totalSize), which is pretty crappy */
    def totalSizeSlow: Int = $.values.map(_.length).sum
  }
  implicit class richTraversableMap[K, A]($: Map[K, Traversable[A]]) extends ToFoldableOps {
    /** Potentially faster for traversables whose size operation is O(1), e.g., Vector or Set. */
    def totalSize: Int = $.values.map(_.size).sum
  }
}
