package common.rich.collections

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.util.Random

import common.rich.RichT._

object RichSeq {
  class __Inserter[T] private[RichSeq]($: Seq[T], elementToInsert: T) {
    /**
     * @param index the index to insert at
     * @throws IndexOutOfBoundsException
     */
    def at(index: Int): Seq[T] = {
      require(index >= 0)
      if ($.size < index)
        throw new IndexOutOfBoundsException(s"requested to remove at $index when size is ${$.size}")
      else {
        val xs = $ splitAt index
        val buffer = xs._1.to(ListBuffer) += elementToInsert
        buffer.++(xs._2).toVector
      }
    }
    def after(index: Int): Seq[T] = at(index + 1)
    def before(index: Int): Seq[T] = at(index - 1)
  }
  implicit class richSeq[T](private val $: Seq[T]) extends AnyVal {
    /** Returns a random shuffle of this sequence in O(n), using the fisher-yates algorithm */
    def shuffle(random: Random): Seq[T] = {
      val array = ArrayBuffer[T]($: _*)
      for (n <- array.length - 1 to 0 by -1) {
        val k = random.nextInt(n + 1)
        val (a, b) = (array(n), array(k))
        array(k) = a
        array(n) = b
      }
      array.toVector
    }
    def shuffle: Seq[T] = shuffle(Random)

    /** Returns a sample of uniformly random n elements */
    def sample(n: Int, random: Random = Random): Seq[T] = {
      require(n <= $.size, s"Can't sample $n items out of a sequence of size ${$.size}")
      def swap(v: Vector[T], a: Int, b: Int): Vector[T] = {
        val (x, y) = v(a) -> v(b)
        v.updated(b, x).updated(a, y)
      }
      @tailrec
      def aux(n: Int, v: Vector[T], result: List[T]): Seq[T] = n match {
        case 0 => result
        case _ =>
          val swapped = swap(v, 0, random.nextInt(v.size))
          aux(n - 1, swapped drop 1, swapped.head :: result)
      }
      aux(n, $.toVector, Nil)
    }

    /** Same as indexWhere, but returns an option instead of -1 */
    def findIndex(pred: T => Boolean): Option[Int] = findWithIndex(pred).map(_._2)

    /** Same as findIndex, but also returns the element found. This can be more efficient (O(n) vs O(2*n)) if the sequence isn't indexed. */
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
     * @param shiftSize the number of elements to shift by. It can be either negative or positive.
     */
    def shift(shiftSize: Int): Seq[T] = $ splitAt shiftSize mapTo (e => e._2 ++ e._1)

    /** All shifts iterators. */
    def shifts: Iterator[Seq[T]] = Iterator.range(0, $.size) map shift

    /** Removes the element at index i. O(n) complexity */
    def removeAt(i: Int): Seq[T] = {
      require(i >= 0)
      if ($.size <= i)
        throw new IndexOutOfBoundsException(s"requested to remove at $i when size is ${$.size}")
      else
        $.splitAt(i).mapTo(e => e._1.to(ListBuffer).++(e._2.drop(1)).toVector)
    }

    /** Prepends an element to the sequence. Unless the underlying sequence is a list, the complexity is O(n). */
    def ::[U >: T](e: U): Seq[U] = e :: $.toList

    /**
     * Inserts the element at the index. O(n) complexity.
     *
     * @param e     the element to insert
     * @param index the index to insert at
     * @throws IndexOutOfBoundsException
     */
    def insertAt(e: T, index: Int): Seq[T] = this insert e at index

    /**
     * Syntactic sugar, so one can write <code>insert e at i</code> or
     * <code>insert e after i</code> or <code>insert e before i</code>
     *
     * @param e the element to insert
     */
    def insert(e: T) = new __Inserter($, e)

    def cutoffsAt(p: T => Boolean): Seq[Seq[T]] = $.foldLeft(Seq[Seq[T]]())((agg, t) => agg match {
      case Nil => List(List(t))
      case x :: xs => if (p(t)) List(t) :: x.reverse :: xs else (t :: x) :: xs
    }).reverse
  }

  implicit class richSeqTuplesDouble[T, S](private val $: Seq[(T, S)]) extends AnyVal {
    def flatZip[U](other: Seq[U]): Seq[(T, S, U)] = $ zip other map (e => (e._1._1, e._1._2, e._2))
    /**
     * Creates a map view from T to S. The map has linear search time, but it keeps the same sequence as the
     * original
     */
    def asMap: Map[T, S] = new Map[T, S]() {
      override def get(key: T): Option[S] = $.find(_._1 == key).map(_._2)
      override def iterator: Iterator[(T, S)] = $.iterator
      override def removed(key: T): Map[T, S] = $.filter(_ != key).asMap
      override def toSeq = $
      override def updated[V1 >: S](key: T, value: V1) =
        $.map {case (x, y) => if (x == key) x -> value else x -> y}.asMap
    }
    def toMultiMap: Map[T, Seq[S]] = RichIterableOnce.richIterableOnce($).toMultiMap(_._1, _._2)
  }

  implicit class richSeqTuplesTriplets[T, S, U](private val $: Seq[(T, S, U)]) extends AnyVal {
    def flatZip[W](other: Seq[W]): Seq[(T, S, U, W)] = $ zip other map (e => (e._1._1, e._1._2, e._1._3, e._2))

    def flatZipWithIndex: Seq[(T, S, U, Int)] = $.zipWithIndex.map(e => (e._1._1, e._1._2, e._1._3, e._2))
  }

  implicit class richSeqTuplesQuadruplets[T, S, U, W](private val $: Seq[(T, S, U, W)]) extends AnyVal {
    def flatZip[X](other: Seq[X]): Seq[(T, S, U, W, X)] = $ zip other map (e => (e._1._1, e._1._2, e._1._3, e._1._4, e._2))

    def flatZipWithIndex: Seq[(T, S, U, W, Int)] = $.zipWithIndex.map(e => (e._1._1, e._1._2, e._1._3, e._1._4, e._2))
  }

  implicit class richNestedSeq[T: ClassTag]($: Seq[Seq[T]]) {
    def deepArray: Array[Array[T]] = $.map(_.toArray).toArray
    def apply(e: (Int, Int)): T = $(e._1)(e._2)
  }
}
