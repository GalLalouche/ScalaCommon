package common.rich.collections

import java.util

import scala.language.higherKinds

import common.rich.func.ToMoreFoldableOps._
import scalaz.{Foldable, Plus, Semigroup}
import scalaz.std.option.optionInstance
import scalaz.syntax.foldable.ToFoldableOps
import scalaz.syntax.semigroup.ToSemigroupOps

import common.rich.RichT._

object RichMap {
  implicit class richJavaMap[K, V](private val $ : util.Map[K, V]) extends AnyVal {
    def getOrPutIfAbsent(k: K, v: => V): V = $.computeIfAbsent(k, _ => v)
    def getOpt(k: K): Option[V] = $.get(k).opt
  }

  implicit class richMap[K, V](private val $ : Map[K, V]) extends AnyVal {
    /** Doesn't return a view. */
    def properMapValues[V2](f: V => V2): Map[K, V2] = RichMapSpecVer.properMapValues($, f)
    /** Throws on duplicate keys. */
    def mapKeys[K2](f: K => K2): Map[K2, V] =
      $.foldLeft(Map[K2, V]()) { case (map, (k, v)) =>
        val k2 = f(k)
        if (map contains k2)
          throw new UnsupportedOperationException(
            s"key <$k2> already exists in map but it is also transformed from key <$k>",
          )
        map + (k2 -> v)
      }
  }

  implicit class richSemigroupMap[K, V: Semigroup]($ : Map[K, V]) {
    def mergeIntersecting(other: Map[K, V]): Map[K, V] =
      RichMapSpecVer.mergeIntersectingSemigroup($, other)
    def merge(other: Map[K, V]): Map[K, V] = other.foldLeft($)(_ upsert _)
    def upsert[V2 >: V: Semigroup](kv: (K, V2)): Map[K, V2] = upsert(kv._1, kv._2)
    def upsert[V2 >: V: Semigroup](k: K, v: => V2): Map[K, V2] =
      $.updated(k, $.get(k).asInstanceOf[Option[V2]].mapHeadOrElse(_ ⊹ v, v))
  }
  implicit class richPlusMap[K, V[_]: Plus, A]($ : Map[K, V[A]]) {
    private val asSemigroup = richSemigroupMap($)(Plus[V].semigroup)
    def merge(other: Map[K, V[A]]): Map[K, V[A]] = asSemigroup.merge(other)
    def mergeIntersecting(other: Map[K, V[A]]): Map[K, V[A]] =
      richSemigroupMap($)(Plus[V].semigroup).mergeIntersecting(other)
  }
  implicit class richFoldableMap[K, V[_]: Foldable, A]($ : Map[K, V[A]]) {
    def flattenValues: Seq[(K, A)] = $.toSeq.flatMap(e => e._2.toVector.map(e._1.->))
    /** Complexity: O(totalSize), which is pretty crappy */
    def totalSizeSlow: Int = $.values.map(_.length).sum
  }
  implicit class richTraversableMap[K, A](private val $ : Map[K, Traversable[A]]) extends AnyVal {
    /** Potentially faster for traversables whose size operation is O(1), e.g., Vector or Set. */
    def totalSize: Int = $.values.map(_.size).sum
  }
}
