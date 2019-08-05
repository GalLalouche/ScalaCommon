package common.rich.collections

import scala.collection.IterableOnce
import scala.math.log10

import scalaz.{Functor, Semigroup}
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.vector.vectorInstance

import common.rich.RichT._
import common.rich.RichTuple._
import common.rich.collections.RichMap._
import common.rich.primitives.RichBoolean._

object RichIterableOnce {
  class __Joiner[A, B] private[RichIterableOnce]($: IterableOnce[A], other: IterableOnce[B]) {
    def where(predicate: (A, B) => Boolean): IterableOnce[(A, B)] =
      for (i <- $; j <- other; if predicate(i, j)) yield (i, j)

    def by[C](ft: A => C, fs: B => C): IterableOnce[(A, B)] = by(ft, fs, (x, y) => (x, y))

    def by[C, D](ft: A => C, fs: B => C, builder: (A, B) => D): IterableOnce[D] = {
      val tMap = $.iterator.map(e => ft(e) -> e).iterator.toMap
      val sMap = other.iterator.map(e => fs(e) -> e).iterator.toMap
      tMap.keys.filter(sMap.contains).map(k => builder(tMap(k), sMap(k)))
    }
  }

  implicit class richIterableOnce[A](private val $: IterableOnce[A]) extends AnyVal {
    def filterNot(p: A => Boolean): IterableOnce[A] = $.iterator.filter(!p(_))

    def reduceByKey[Key](toKey: A => Key)(implicit ev: Semigroup[A]): Map[Key, A] =
      aggregateMap(toKey, identity)
    def aggregateMap[Key, Value: Semigroup](toKey: A => Key, toValue: A => Value): Map[Key, Value] =
      $.iterator.map(_.toTuple(toKey, toValue)).foldLeft(Map[Key, Value]())(_ upsert _)

    /** Throws on repeat keys. */
    def mapBy[B](f: A => B): Map[B, A] =
      $.iterator.foldLeft(Map[B, A]())((map, nextValue) => {
        val key = f(nextValue)
        if (map.contains(key))
          throw new IllegalArgumentException(
            s"key <$key> is already used for value <${map(key)}>, " +
                s"but is also requested as a key for value <$nextValue>")
        map + (key -> nextValue)
      })

    /**
     * Performs a foreach iteration, running a function between each two items.
     * Can be thought of as a side-effect-full alternative to mkString.
     *
     * @param f       the function to apply to the elements
     * @param between the function to apply between elements
     */
    def foreachWithBetween(f: A => Unit, between: () => Unit) {
      val iterator = $.iterator
      while (iterator.hasNext) {
        f(iterator.next())
        if (iterator.hasNext)
          between()
      }
    }

    def toMultiMap[S](f: A => S): Map[S, Seq[A]] = toMultiMap(f, identity)
    def toMultiMap[S, U](toKey: A => S, toValue: A => U): Map[S, Seq[U]] =
      aggregateMap(toKey, toValue(_) :: Nil)

    /** The number of occurrences of each element */
    def frequencies: Map[A, Int] = aggregateMap(e => e, 1.const)

    def entropy: Double = {
      val f = frequencies
      val size = f.values.sum
      f.values
          .map(_.toDouble / size)
          .map(p => -p * log10(p) / log10(2))
          .sum
    }

    /** Retrieves all <i>N choose 2<\i> pairs */
    def unorderedPairs: IterableOnce[(A, A)] = {
      val withIndex = $.iterator.toVector.zipWithIndex
      for ((a, i) <- withIndex; (b, j) <- withIndex; if i < j) yield a -> b
    }

    def hasSameValues[U](f: A => U): Boolean = {
      val iterator = $.iterator
      val sample = f(iterator.next())
      iterator.map(f).forall(_ == sample)
    }

    /** Checks if the iterable has any repeats */
    def allUnique: Boolean =
      $.iterator.scanLeft(Set[A]())(_ + _).zipWithIndex.forall(_.reduce(_.size == _))

    /** Finds the percentage of elements satisfying the predicate */
    def percentageSatisfying(p: A => Boolean): Double = {
      val (total, satisfy) = $.iterator.foldLeft((0, 0)) {
        (agg, next) => (agg._1 + 1, agg._2 + (if (p(next)) 1 else 0))
      }
      satisfy.toDouble / total
    }

    /** Returns the Cartesian product of both iterables. */
    def *[B](ys: IterableOnce[B]): IterableOnce[(A, B)] = {
      val yIterable = ys.iterator.to(Iterable) // Needed if y really is iterable*Once*.
      for (x <- $; y <- yIterable) yield (x, y)
    }

    /** Selects a representative from each equivalence set */
    def selectRepresentative[B](f: A => B): IterableOnce[A] = {
      implicit val semigroupFirst: Semigroup[A] = Semigroup.instance((x, _) => x)
      aggregateMap(f, e => e).values
    }

    def join[B](other: IterableOnce[B]) = new __Joiner($, other)

    def single: A = {
      val i = $.iterator
      val next = i.next()
      if (i.hasNext)
        throw new UnsupportedOperationException("Iterable contained more than a single element")
      next
    }

    def contains(a: A): Boolean = $.iterator.contains(a)

    def filterAndSortBy[B](f: A => B, order: Seq[B]): Seq[A] = {
      val orderMap = order.zipWithIndex.toMap
      Functor[Vector].fproduct($.iterator.toVector)(f)
          .filter(_._2 |> orderMap.contains)
          .sortBy(_._2 |> orderMap)
          .map(_._1)
    }

    def fornone(p: A => Boolean): Boolean = $.iterator.exists(p).isFalse
    def existsNot(p: A => Boolean): Boolean = $.iterator.forall(p).isFalse

    def range(implicit ev: Ordering[A]): (A, A) = {
      val i = $.iterator
      val initial = i.next()
      i.foldLeft(initial -> initial) {case (agg, next) => ev.min(next, agg._1) -> ev.max(next, agg._2)}
    }
  }
}
