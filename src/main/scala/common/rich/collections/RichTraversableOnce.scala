package common.rich.collections

import scala.collection.mutable
import scala.math.log10
import scalaz.Semigroup
import scalaz.std.AllInstances._

object RichTraversableOnce {
  implicit class richTraversableOnce[T]($: TraversableOnce[T]) {
    def aggregateMap[Key, Value: Semigroup](toKey: T => Key, toValue: T => Value) =
      $.foldLeft(Map[Key, Value]()) { (m, next) =>
        val key = toKey(next)
        val value = toValue(next)
        m + (key -> m.get(key).map(implicitly[Semigroup[Value]].append(_, value)).getOrElse(value))
      }

    def mapBy[S](f: T => S): Map[S, T] = {
      val map = new mutable.HashMap[S, T]
      for (t <- $) {
        val key = f(t)
        if (map.contains(key))
          throw new UnsupportedOperationException(
            s"key <$key> is already used for value <${map(key)}>, but is also requested as a key for value <$t>")
        map += key -> t
      }
      map.toMap
    }

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

    def toMultiMap[S](f: T => S): Map[S, Seq[T]] = aggregateMap(f, _ :: Nil)

    /** The number of occurrences of each element */
    def frequencies: Map[T, Int] = aggregateMap(e => e, e => 1)

    /** The entropy value of this traversable */
    def entropy: Double = {
      val f = frequencies
      val size = f.values.sum
      frequencies.values
        .map(_.toDouble / size)
        .map(p => -p * log10(p) / log10(2))
        .sum
    }

    /** Retrieves all <i>N choose 2<\i> pairs */
    def unorderedPairs: TraversableOnce[(T, T)] = {
      val withIndex = $.toSeq.zipWithIndex
      withIndex * withIndex filter (e => e._1._2 < e._2._2) map (e => e._1._1 -> e._2._1)
    }

    def hasSameValues[U](f: T => U): Boolean = {
      val iterator = $.toIterator
      val sample = f(iterator.next())
      iterator.map(f).forall(_ == sample)
    }

    /** Checks if the traversable has any repeats */
    def allUnique: Boolean = $.toSet.size == $.size

    /** Finds the percentage of elements satisfying the predicate */
    def percentageSatisfying(p: T => Boolean): Double = {
      var satisfy = 0
      var total = 0
      for (t <- $) {
        total += 1
        if (p(t))
          satisfy += 1
      }
      satisfy.toDouble / total
    }

    /**
      * Returns the Cartesian product of both sequences.
      * @param xs the other traversable
      */
    def *[S](xs: TraversableOnce[S]): TraversableOnce[(T, S)] = for (x <- $; y <- xs) yield (x, y)

    /** Selects a representative from each equivalence set */
    def selectRepresentative[U](f: T => U): TraversableOnce[T] = {
      implicit val semigroupFirst = new Semigroup[T] {
        override def append(f1: T, f2: => T): T = f1
      }
      aggregateMap(f, e => e).values
    }

    def join[S](other: TraversableOnce[S]) = new {
      def where(predicate: (T, S) => Boolean): Seq[(T, S)] =
        (for (i <- $; j <- other; if predicate(i, j)) yield (i, j)).toVector
      def by[U](ft: T => U, fs: S => U): Seq[(T, S)] = by(ft, fs, (x, y) => (x, y))
      def by[U, W](ft: T => U, fs: S => U, builder: (T, S) => W): Seq[W] =
        where((t, s) => ft(t) == fs(s)).map(e => builder(e._1, e._2))
    }

    def single: T = {
      var result: Option[T] = None
      $.foreach { t =>
        if (result == None) result = Some(t)
        else throw new UnsupportedOperationException("Traversable contained more than a single element") }
      result.get
    }
  }
}
