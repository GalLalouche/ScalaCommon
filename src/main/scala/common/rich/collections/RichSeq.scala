package common.rich.collections

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

import common.rich.RichT._
import common.rich.RichTuple._
import common.rich.collections.RichIterable.richIterable
import common.rich.collections.RichIterator.richIterator
import common.rich.primitives.RichBoolean

object RichSeq {
  class __Inserter[T] private[RichSeq] ($ : Seq[T], elementToInsert: T) {
    /**
     * @param index
     *   the index to insert at
     * @throws IndexOutOfBoundsException
     */
    def at(index: Int): Seq[T] = {
      require(index >= 0)
      if ($.size < index)
        throw new IndexOutOfBoundsException(s"requested to remove at $index when size is ${$.size}")
      else $.splitAt(index).thrush(xs => (xs._1.to[ArrayBuffer] += elementToInsert) ++ xs._2)
    }
    def after(index: Int): Seq[T] = at(index + 1)
    def before(index: Int): Seq[T] = at(index - 1)
  }
  implicit class richSeq[T](private val $ : Seq[T]) extends AnyVal {
    /** Returns a random shuffle of this sequence in O(n), using the fisher-yates algorithm */
    def shuffle(random: Random): Seq[T] = {
      val array = ArrayBuffer[T]($ : _*)
      for (n <- array.length - 1 to 0 by -1) {
        val k = random.nextInt(n + 1)
        val temp = array(k)
        array(k) = array(n)
        array(n) = temp
      }
      array.toVector
    }
    def shuffle: Seq[T] = shuffle(Random)

    /** Returns a sample of uniformly random n elements */
    def sample(n: Int, random: Random = Random): Seq[T] = shuffle(random).take(n)

    def firstSome[S](f: T => Option[S]): Option[S] = $.iterator.flatMap(f(_)).headOption()

    /** Same as indexWhere, but returns an option instead of -1 */
    def findIndex(pred: T => Boolean): Option[Int] = findWithIndex(pred).map(_._2)

    /**
     * Same as findIndex, but also returns the element found. This can be more efficient (O(n) vs
     * O(2*n)) if the sequence isn't indexed.
     */
    def findWithIndex(pred: T => Boolean): Option[(T, Int)] = {
      // the below implementation is quicker for non-indexed Seqs
      var i = 0
      for (t <- $) {
        if (pred(t))
          return Some((t, i))
        i += 1
      }
      None
    }

    /**
     * Cyclicly shifts the sequence
     *
     * @param shiftSize
     *   the number of elements to shift by. It can be either negative or positive.
     */
    def shift(shiftSize: Int): Seq[T] = $.splitAt(shiftSize).thrush(e => e._2 ++ e._1)

    /** All shifts iterators. */
    def shifts: Iterator[Seq[T]] = Iterator.range(0, $.size).map(shift)

    /** Removes the element at index i. O(n) complexity */
    def removeAt(i: Int): Seq[T] = {
      require(i >= 0)
      if ($.size <= i)
        throw new IndexOutOfBoundsException(s"requested to remove at $i when size is ${$.size}")
      else
        $.splitAt(i).|>(e => e._1.to[ArrayBuffer] ++ e._2.drop(1))
    }

    /**
     * Inserts the element at the index. O(n) complexity.
     *
     * @param e
     *   the element to insert
     * @param index
     *   the index to insert at
     * @throws IndexOutOfBoundsException
     */
    def insertAt(e: T, index: Int): Seq[T] = this.insert(e).at(index)

    /**
     * Syntactic sugar, so one can write <code>insert e at i</code> or <code>insert e after i</code>
     * or <code>insert e before i</code>
     *
     * @param e
     *   the element to insert
     */
    def insert(e: T) = new __Inserter($, e)

    def cutoffsAt(p: T => Boolean): Seq[Seq[T]] = $.foldLeft(List[List[T]]())((agg, t) =>
      agg match {
        case Nil => List(List(t))
        case x :: xs => if (p(t)) List(t) :: x.reverse :: xs else (t +: x) :: xs
      },
    ).reverse

    def pairSliding: Iterator[(T, T)] =
      if ($.size <= 1) Iterator.empty else $.sliding(2).map(e => e(0) -> e(1))

    /** Throws [[NoSuchElementException]] if no element satisfies the predicate. */
    def takeUntilIncluding(predicate: T => Boolean): Seq[T] =
      $.toStream.span(RichBoolean.negate(predicate)).reduce(_ :+ _.head).toVector

    /** Throws [[NoSuchElementException]] if sequences aren't the same length. */
    def safeZipWith[S, W](other: Seq[S])(f: (T, S) => W): Seq[W] = {
      val i1 = $.iterator
      val i2 = other.iterator
      val b = $.genericBuilder[W]
      while (i1.hasNext && i2.hasNext)
        b += f(i1.next(), i2.next())
      if (i1.hasNext || i2.hasNext)
        throw new NoSuchElementException(
          s"${if (i1.hasNext) "Left" else "Right"} side still has values",
        )
      b.result()
    }

    def intersperse(a: => T): Seq[T] =
      $.mapIf(_.hasAtLeastSizeOf(2)).to($.head +: $.tail.flatMap(Vector(a, _)))

    /** Like groupBy, but retains the order of the elements encountered */
    def orderedGroupBy[S](f: T => S): Seq[(S, Seq[T])] = {
      val result = new mutable.LinkedHashMap[S, mutable.Buffer[T]]
      $.foreach(x => result.getOrElseUpdate(f(x), new ArrayBuffer[T]) += x)
      result.mapValues(_.toVector).toVector
    }
  }

  implicit class richSeqTuplesDouble[T, S](private val $ : Seq[(T, S)]) extends AnyVal {
    def flatZip[U](other: Seq[U]): Seq[(T, S, U)] = $.zip(other).map(e => (e._1._1, e._1._2, e._2))
    /**
     * Creates a map view from T to S. The map has linear search time, but on the other hand it
     * keeps the same order as the original.
     */
    def asMap: Map[T, S] = new Map[T, S]() {
      override def +[B1 >: S](kv: (T, B1)): Map[T, B1] = throw new UnsupportedOperationException
      override def get(key: T): Option[S] = $.find(_._1 == key).map(_._2)
      override def iterator: Iterator[(T, S)] = $.iterator
      override def -(key: T): Map[T, S] = throw new UnsupportedOperationException
      override def toSeq = $
    }
    def toMultiMap: Map[T, Seq[S]] =
      RichTraversableOnce.richTraversableOnce($).toMultiMap(_._1, _._2)
  }

  implicit class richSeqTuplesTriplets[T, S, U](private val $ : Seq[(T, S, U)]) extends AnyVal {
    def flatZip[W](other: Seq[W]): Seq[(T, S, U, W)] =
      $.zip(other).map(e => (e._1._1, e._1._2, e._1._3, e._2))

    def flatZipWithIndex: Seq[(T, S, U, Int)] =
      $.zipWithIndex.map(e => (e._1._1, e._1._2, e._1._3, e._2))
  }

  implicit class richSeqTuplesQuadruplets[T, S, U, W](private val $ : Seq[(T, S, U, W)])
      extends AnyVal {
    def flatZip[X](other: Seq[X]): Seq[(T, S, U, W, X)] =
      $.zip(other).map(e => (e._1._1, e._1._2, e._1._3, e._1._4, e._2))

    def flatZipWithIndex: Seq[(T, S, U, W, Int)] =
      $.zipWithIndex.map(e => (e._1._1, e._1._2, e._1._3, e._1._4, e._2))
  }

  implicit class richNestedSeq[T: ClassTag]($ : Seq[Seq[T]]) {
    def deepArray: Array[Array[T]] = $.map(_.toArray).toArray
    def apply(e: (Int, Int)): T = $(e._1)(e._2)
  }
}
