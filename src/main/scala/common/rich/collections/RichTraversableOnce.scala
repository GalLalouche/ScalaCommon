package common.rich.collections

import common.rich.RichT._
import common.rich.RichTuple._
import common.rich.collections.RichMap._
import common.rich.primitives.RichBoolean._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.vector.vectorInstance
import scalaz.{Functor, Semigroup}

import scala.collection.TraversableOnce
import scala.math.log10

object RichTraversableOnce {
  class __Joiner[A, B] private[RichTraversableOnce]($: TraversableOnce[A], other: TraversableOnce[B]) {
    def where(predicate: (A, B) => Boolean): TraversableOnce[(A, B)] =
      for (i <- $; j <- other; if predicate(i, j)) yield (i, j)

    def by[C](ft: A => C, fs: B => C): TraversableOnce[(A, B)] = by(ft, fs, (x, y) => (x, y))

    def by[C, D](ft: A => C, fs: B => C, builder: (A, B) => D): TraversableOnce[D] = {
      val tMap = $.map(e => ft(e) -> e).toMap
      val sMap = other.map(e => fs(e) -> e).toMap
      tMap.keys.filter(sMap.contains).map(k => builder(tMap(k), sMap(k)))
    }
  }

  implicit class richTraversableOnce[A](private val $: TraversableOnce[A]) extends AnyVal {
    def filterNot(p: A => Boolean): TraversableOnce[A] = $.filter(!p(_))

    def reduceByKey[Key](toKey: A => Key)(implicit ev: Semigroup[A]): Map[Key, A] =
      aggregateMap(toKey, identity)
    def aggregateMap[Key, Value: Semigroup](toKey: A => Key, toValue: A => Value): Map[Key, Value] =
      $.map(_.toTuple(toKey, toValue)).foldLeft(Map[Key, Value]())(_ upsert _)

    /** Throws on repeat keys. */
    def mapBy[B](f: A => B): Map[B, A] =
      $.foldLeft(Map[B, A]())((map, nextValue) => {
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
      val iterator = $.toIterator
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
    def unorderedPairs: TraversableOnce[(A, A)] = {
      val withIndex = $.toVector.zipWithIndex
      for ((a, i) <- withIndex; (b, j) <- withIndex; if i < j) yield a -> b
    }

    def hasSameValues[U](f: A => U): Boolean = {
      val iterator = $.toIterator
      val sample = f(iterator.next())
      iterator.map(f).forall(_ == sample)
    }

    /** Checks if the traversable has any repeats */
    def allUnique: Boolean =
      $.toIterator.scanLeft(Set[A]())(_ + _).zipWithIndex.forall(_.reduce(_.size == _))

    /** Finds the percentage of elements satisfying the predicate */
    def percentageSatisfying(p: A => Boolean): Double = {
      val (total, satisfy) = $.foldLeft((0, 0)) {(agg, next) => (agg._1 + 1, agg._2 + (if (p(next)) 1 else 0))}
      satisfy.toDouble / total
    }

    /** Returns the Cartesian product of both traversables. */
    def *[B](ys: TraversableOnce[B]): TraversableOnce[(A, B)] = {
      val yIterable = ys.toIterable // Needed if y really is traversable*Once*.
      for (x <- $; y <- yIterable) yield (x, y)
    }

    /** Selects a representative from each equivalence set */
    def selectRepresentative[B](f: A => B): TraversableOnce[A] = {
      implicit val semigroupFirst: Semigroup[A] = Semigroup.instance((x, _) => x)
      aggregateMap(f, e => e).values
    }

    def join[B](other: TraversableOnce[B]) = new __Joiner($, other)

    def single: A = {
      val i = $.toIterator
      val next = i.next()
      if (i.hasNext)
        throw new UnsupportedOperationException("Traversable contained more than a single element")
      next
    }

    def filterAndSortBy[B](f: A => B, order: Seq[B]): Seq[A] = {
      val orderMap = order.zipWithIndex.toMap
      Functor[Vector].fproduct($.toVector)(f)
          .filter(_._2 |> orderMap.contains)
          .sortBy(_._2 |> orderMap)
          .map(_._1)
    }

    def fornone(p: A => Boolean): Boolean = $.exists(p).isFalse
    def existsNot(p: A => Boolean): Boolean = $.forall(p).isFalse

    def range(implicit ev: Ordering[A]): (A, A) = {
      val i = $.toIterator
      val initial = i.next()
      i.foldLeft(initial -> initial) {case (agg, next) => ev.min(next, agg._1) -> ev.max(next, agg._2)}
    }
  }
}
