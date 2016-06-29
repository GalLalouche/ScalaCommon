package common.rich.collections

import java.lang.Math._

import common.TupleFunction._

import scala.util.Try

class RichTraversable[T]($: Traversable[T]) {
	/** Maps only those that do not throw an exception */
	def tryMap[U](f: T ⇒ U): Seq[U] = $.map(e ⇒ (e, Try(f(e))))
		.filter(_._2.isSuccess)
		.map(_._2.get)
		.toSeq

	
	/** Zips sequence with a mapping of the sequence */
	def zipMap[S](f: T => S) = $.map(e => e -> f(e))
	
	/** Checks if the traversable has any repeats */
	def allUnique: Boolean = $.toSet.size == $.size

	def hasSameValues[U](f: T => U): Boolean = {
		val sample = f($.head)
		$.forall(f(_) == sample)
	}

	/** Retrieves all <i>N choose 2<\i> pairs */
	def getUniquePairs: Traversable[(T, T)] = {
		val withIndex = $.toVector.zipWithIndex
		for (e1 <- withIndex; e2 <- withIndex if e2._2 > e1._2) yield (e1._1, e2._1)
	}

	/**
	 * Performs a foreach iteration, running a function between each two items.
	 * Can be thought of as a side-effect-full alternative to mkString.
	 * @param f the function to apply to the elements
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

	def join[S](other: Traversable[S]) = new {
		def where(predicate: (T, S) => Boolean): Traversable[(T, S)] = {
			for (i <- $; j <- other; if predicate(i, j)) yield (i, j)
		}

		def by[U](ft: T => U, fs: S => U): Traversable[(T, S)] = by(ft, fs, (x, y) => (x, y))

		def by[U, W](ft: T => U, fs: S => U, builder: (T, S) => W): Traversable[W] =
			where((t, s) => ft(t) == fs(s)) map builder
	}

	/** The number of occurrences of each element */
	def frequencies: Map[T, Int] = $ groupBy (e => e) map (e => e._1 -> e._2.size)

	/** The entropy value of this traversable */
	def entropy: Double = {
		val size: Int = $.size
		$
			.groupBy(e => e)
			.values
			.map(_.size.toDouble / size)
			.map(p => -p * log10(p) / log10(2))
			.sum
	}

	/**
	 * Returns the Cartesian product of both sequences.
	 * The order will be (this1, other1), (this1, other2), ..., (this2, other1), ...
	 * @param e the other traversable
	 */
	def *[S](xs: Traversable[S]): Traversable[(T, S)] = for (x ← $; y ← xs) yield (x, y)

	/** Finds the percentage of elements satisfying the predicate */
	def percentageSatisfying(p: T ⇒ Boolean): Double = $.count(p) / $.size.toDouble

	/** Selects a representative from each equivalence set */
	def selectRepresentative[U](f: T => U): Traversable[T] = $ groupBy f map (_._2.head)

	/** Finds an element that is equal to the input */
	def find(e: T): Option[T] = $ find (_ == e)

	def mapDefined[U](f: T => Option[U]): Traversable[U] = $ map f filter (_ isDefined) map (_ get)

	def mapBy[S](f: T => S): Map[S, T] = zipMap(f).map(_.swap).toMap
}

object RichTraversable {
	implicit def richTraversable[T]($: Traversable[T]) = new RichTraversable($)
	implicit def richTraversableOption[T]($: Traversable[Option[T]]) = new {
		def defined = $.view.filter(_.isDefined).map(_.get).force
	}
}
