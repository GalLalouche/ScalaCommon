package common.rich.collections

import java.util
import java.util.{Spliterator, Spliterators}
import java.util.stream.StreamSupport

import scala.annotation.tailrec
import scala.collection.{mutable, AbstractIterator}
import scala.util.Sorting

import common.UtilsVersionSpecific
import common.rich.ConvertersVersionSpecific
import common.rich.RichT._
import common.rich.primitives.RichBoolean._

object RichIterator {
  implicit class richIterator[A](private val $ : Iterator[A]) extends AnyVal {
    /** Returns an iterator that throws an exception on the first item that does not satisfy f */
    def verify(
        f: A => Boolean,
        exceptionMessage: (A, Int) => String = (e, i) => s"Item $e @ $i failed f",
    ): Iterator[A] =
      $.zipWithIndex.map(e =>
        if (f(e._1)) e._1 else throw new Exception(exceptionMessage(e._1, e._2)),
      )

    /**
     * Returns an iterator that outputs to the console its iteration number
     *
     * @param frequency
     *   the frequency of the output, i.e., how often should the message be printed. Default is
     *   every time, i.e., at every step.
     */
    def withCounter(frequency: Int = 1): Iterator[A] =
      withCounter(i => if (i % frequency == 0) Some(i.toString) else None)

    /**
     * Returns an iterator that outputs to the console its progress
     *
     * @param f
     *   A function from iteration number to an optional string. If the None, nothing will be
     *   printed. Otherwise, f(e) will be printed, where e is the current element being processed.
     */
    def withCounter(f: Int => Option[String]): Iterator[A] = new AbstractIterator[A] {
      private var i = 0
      override def hasNext =
        if ($.hasNext)
          true
        else {
          print("\r")
          false
        }
      override def next() = {
        i += 1
        for (l <- f(i))
          print("\r" + l)
        $.next
      }
    }

    /**
     * Returns an iterator that outputs to the console its progress in percentages
     *
     * @param size
     *   the total number of elements in the iterator
     */
    def withPercentage(size: Int): Iterator[A] = {
      var lastPercentage = 0
      withCounter { i =>
        val currentPercentage = i * 100 / size
        if (currentPercentage > lastPercentage) {
          lastPercentage = currentPercentage
          Some(s"$currentPercentage% done")
        } else None
      }
    }

    def zipWithIndex: Iterator[(A, Int)] = new AbstractIterator[(A, Int)] {
      private var i = -1
      override def hasNext = $.hasNext
      override def next() = {
        i += 1
        ($.next, i)
      }
    }

    def reducingIterator(f: (A, A) => A): Iterator[A] =
      if ($.isEmpty) Iterator() else $.scanLeft($.next)(f)

    /**
     * Similar to takeWhile, except the first element not satisfying the predicate is also included.
     */
    def takeUntil(p: A => Boolean): Iterator[A] = new AbstractIterator[A] {
      private var stopped: Boolean = false
      override def hasNext = stopped.isFalse && $.hasNext
      override def next() = {
        if (stopped) throw new NoSuchElementException
        val result = $.next()
        stopped = p(result).isFalse
        result
      }
    }

    def last(): A = {
      @tailrec
      def go(current: A): A = if ($.hasNext) go($.next()) else current
      go($.next())
    }

    def apply(n: Int): A = {
      val dropped = $.drop(n)
      if (dropped.hasNext)
        dropped.next()
      else
        throw new IndexOutOfBoundsException(s"<$n>")
    }
    def headOption(): Option[A] = $.optMap(_.hasNext, _.next())
    def lastOption(): Option[A] = if ($.hasNext) Some(last()) else None
    def lazyFoldl[B](b: B)(f: (A, B) => Option[B]): B = {
      @tailrec def go(agg: B): B =
        if ($.hasNext)
          f($.next(), agg) match {
            case None => agg
            case Some(n) => go(n)
          }
        else
          agg
      go(b)
    }

    def groupBy[Key](f: A => Key): collection.Map[Key, Iterable[A]] = {
      val result = scala.collection.mutable.Map[Key, mutable.Buffer[A]]()
      $.foreach { e =>
        result.getOrElseUpdate(f(e), scala.collection.mutable.ArrayBuffer[A]()) += e
      }
      result
    }
    /** Returns an [[Array]] backed [[Seq]], i.e., not persistent, but is immutable. */
    def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Seq[A] =
      // There's a lot of terrible "unsafe" casting here for the sake of performance/convenience.
      // 1. We're using arrays directly here for performance, rather than going through a Vector first.
      //    Since this array never escapes this scope, so we can wrap with WrappedArray.
      // 2. We want to avoid passing in an implicit ClassTag[A] just for array creation, since we're
      //    already passing an implicit Ord (which would passing an explicit Ord annoying). Arrays
      //    are covariant, so we can create an Array[Any] and cast it to Array[A] when sorting.
      UtilsVersionSpecific.unsafeArray(
        $.toArray[Any].asInstanceOf[Array[A]].<|(Sorting.quickSort(_)(ord.on(f))),
      )
    def toJavaStream: util.stream.Stream[A] = ConvertersVersionSpecific.toJava($).toStream
  }

  implicit class richJavaIterator[A](private val $ : util.Iterator[A]) extends AnyVal {
    def toStream: util.stream.Stream[A] = StreamSupport.stream(
      Spliterators.spliteratorUnknownSize($, Spliterator.ORDERED),
      false,
    )
  }

  def iterateOptionally[A](a: A)(f: A => Option[A]): Iterator[A] =
    Iterator.iterate(Option(a))(f apply _.get).takeWhile(_.isDefined).map(_.get)
  @tailrec def farthest[A](a: A)(f: A => Option[A]): A = {
    val $ = f(a)
    if ($.isDefined) farthest($.get)(f) else a
  }
}
