package common.rich.collections

import common.rich.primitives.RichBoolean._
import common.rich.RichT._
import common.rich.collections.RichMap._
import common.rich.func.ToMoreFoldableOps

import scala.collection.TraversableOnce
import scala.math.log10

import scalaz.{Functor, Semigroup}
import scalaz.std.{AnyValInstances, ListInstances, OptionInstances, VectorInstances}

object RichTraversableOnce
    extends ToMoreFoldableOps
        with VectorInstances with ListInstances with AnyValInstances with OptionInstances {
  class __Joiner[T, S] private[RichTraversableOnce]($: TraversableOnce[T], other: TraversableOnce[S]) {
    def where(predicate: (T, S) => Boolean): TraversableOnce[(T, S)] =
      for (i <- $; j <- other; if predicate(i, j)) yield (i, j)

    def by[U](ft: T => U, fs: S => U): TraversableOnce[(T, S)] = by(ft, fs, (x, y) => (x, y))

    def by[U, W](ft: T => U, fs: S => U, builder: (T, S) => W): TraversableOnce[W] = {
      val tMap = $.map(e => ft(e) -> e).toMap
      val sMap = other.map(e => fs(e) -> e).toMap
      tMap.keys.filter(sMap.contains).map(k => builder(tMap(k), sMap(k)))
    }
  }

  implicit class richTraversableOnce[T](private val $: TraversableOnce[T]) extends AnyVal {
    def reduceByKey[Key](toKey: T => Key)(implicit ev: Semigroup[T]): Map[Key, T] =
      aggregateMap(toKey, identity)
    def aggregateMap[Key, Value: Semigroup](toKey: T => Key, toValue: T => Value): Map[Key, Value] =
      $.map(_.toTuple(toKey, toValue)).foldLeft(Map[Key, Value]())(_ upsert _)

    /** Throws on repeat keys. */
    def mapBy[S](f: T => S): Map[S, T] =
      $.foldLeft(Map[S, T]())((map, nextValue) => {
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
    def foreachWithBetween(f: T => Unit, between: () => Unit) {
      val iterator = $.toIterator
      while (iterator.hasNext) {
        f(iterator.next())
        if (iterator.hasNext)
          between()
      }
    }

    def toMultiMap[S](f: T => S): Map[S, Seq[T]] = toMultiMap(f, identity)
    def toMultiMap[S, U](toKey: T => S, toValue: T => U): Map[S, Seq[U]] =
      aggregateMap(toKey, toValue(_) :: Nil)

    /** The number of occurrences of each element */
    def frequencies: Map[T, Int] = aggregateMap(e => e, 1.const)

    /** The entropy value of this traversable */
    def entropy: Double = {
      val f = frequencies
      val size = f.values.sum
      f.values
          .map(_.toDouble / size)
          .map(p => -p * log10(p) / log10(2))
          .sum
    }

    /** Retrieves all <i>N choose 2<\i> pairs */
    def unorderedPairs: TraversableOnce[(T, T)] = {
      val withIndex = $.toVector.zipWithIndex
      for ((a, i) <- withIndex; (b, j) <- withIndex; if i < j) yield a -> b
    }

    def hasSameValues[U](f: T => U): Boolean = {
      val iterator = $.toIterator
      val sample = f(iterator.next())
      iterator.map(f).forall(_ == sample)
    }

    /** Checks if the traversable has any repeats */
    def allUnique: Boolean =
      $.toIterator.scanLeft(Set[T]())(_ + _).zipWithIndex.forall {case (s, i) => s.size == i}

    /** Finds the percentage of elements satisfying the predicate */
    def percentageSatisfying(p: T => Boolean): Double = {
      val (total, satisfy) = $.foldLeft((0, 0)) {(agg, next) => (agg._1 + 1, agg._2 + (if (p(next)) 1 else 0))}
      satisfy.toDouble / total
    }

    /** Returns the Cartesian product of both traversables. */
    def *[S](ys: TraversableOnce[S]): TraversableOnce[(T, S)] = {
      val yIterable = ys.toIterable // Needed if y really is traversable*Once*.
      for (x <- $; y <- yIterable) yield (x, y)
    }

    /** Selects a representative from each equivalence set */
    def selectRepresentative[U](f: T => U): TraversableOnce[T] = {
      implicit val semigroupFirst: Semigroup[T] = Semigroup.instance((x, _) => x)
      aggregateMap(f, e => e).values
    }

    def join[S](other: TraversableOnce[S]) = new __Joiner($, other)

    def single: T = {
      val i = $.toIterator
      val next = i.next()
      if (i.hasNext)
        throw new UnsupportedOperationException("Traversable contained more than a single element")
      next
    }

    def filterAndSortBy[S](f: T => S, order: Seq[S]): Seq[T] = {
      val orderMap = order.zipWithIndex.toMap
      Functor[Vector].fproduct($.toVector)(f)
          .filter(_._2 |> orderMap.contains)
          .sortBy(_._2 |> orderMap)
          .map(_._1)
    }

    def fornone(p: T => Boolean): Boolean = $.exists(p).isFalse
    def existsNot(p: T => Boolean): Boolean = $.forall(p).isFalse
  }
}
