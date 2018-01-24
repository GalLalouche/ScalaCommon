package common.rich.collections

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ParSeq

object RichIterator {
  private class ParIterator[T]($: Iterator[T], windowSize: Int) extends Iterator[T] {
    override val seq = $
    override def hasNext = $.hasNext
    override def next() = $.next()

    // Some iterators implement take() in a side-effect-less free way, so this method explicitly calls next().
    private def take(): ParSeq[T] = {
      val v = new ArrayBuffer[T](windowSize)
      var i = 0
      while (hasNext && i < windowSize) {
        v += next()
        i += 1
      }
      v.toVector.par
    }
    override def map[U](f: T => U): Iterator[U] =
      (take() map f).iterator ++ (if (hasNext) new ParIterator($, windowSize) map f else Iterator.empty)

    override def foreach[U](f: T => U) {
      while ($.hasNext)
        take() foreach f
    }
  }

  implicit class richIterator[T]($: Iterator[T]) {
    /** Returns an iterator that throws an exception on the first item that does not satisfy f */
    def verify(f: T => Boolean,
        exceptionMessage: (T, Int) => String = (e, i) => s"Item $e @ $i failed f"): Iterator[T] =
      $.zipWithIndex.map(e => if (f(e._1)) e._1 else throw new Exception(exceptionMessage(e._1, e._2)))

    /**
     * Returns an iterator that outputs to the console its iteration number
     *
     * @param frequency the frequency of the output, i.e., how often should the message be printed.
     *                  Default is every time, i.e., at every step.
     */
    def withCounter(frequency: Int = 1): Iterator[T] = withCounter(i => if (i % frequency == 0) Some(i.toString) else None)

    /**
     * Returns an iterator that outputs to the console its progress
     *
     * @param f A function from iteration number to an optional string.
     *          If the None, nothing will be printed.
     *          Otherwise, f(e) will be printed, where e is the current element being processed.
     */
    def withCounter(f: Int => Option[String]): Iterator[T] = new Iterator[T] {
      private var i = 0
      override def hasNext = {
        if ($.hasNext)
          true
        else {
          print("\r")
          false
        }

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
     * @param size the total number of elements in the iterator
     */
    def withPercentage(size: Int): Iterator[T] = {
      var lastPercentage = 0
      withCounter(i => {
        val currentPercentage = i * 100 / size
        if (currentPercentage > lastPercentage) {
          lastPercentage = currentPercentage
          Some(s"$currentPercentage% done")
        } else None
      })
    }

    def zipWithIndex: Iterator[(T, Int)] = new Iterator[(T, Int)] {
      private var i = -1
      override def hasNext = $.hasNext
      override def next() = {
        i += 1
        ($.next, i)
      }
    }

    def par(windowSize: Int = 20): Iterator[T] = new ParIterator($, windowSize)
  }
}
