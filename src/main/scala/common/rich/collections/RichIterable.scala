package common.rich.collections

import common.rich.ConvertersVersionSpecific
import common.rich.collections.RichIterator.richIterator

object RichIterable {
  implicit class richIterable[A](private val $ : Iterable[A]) extends AnyVal {
    // Same method as in Seq, used by infinite Iterables
    // This method doesn't exist in RichIterator (or RichTraversableOnce) since checking single-time
    // consumable length consumes it, i.e., it has side-effects.
    def lengthCompare(n: Int): Int = RichIterableVersionSpecific.lengthCompare($, n)

    def hasAtLeastSizeOf(n: Int): Boolean = $.lengthCompare(n) >= 0
    def isLargerThan(n: Int): Boolean = $.lengthCompare(n) > 0
    def hasAtMostSizeOf(n: Int): Boolean = $.lengthCompare(n) <= 0
    def isSmallerThan(n: Int): Boolean = $.lengthCompare(n) < 0
    def hasExactlySizeOf(n: Int): Boolean = $.lengthCompare(n) == 0
    /** Like lengthCompare, but nicer return value. */
    def checkLength(n: Int): CheckLengthResult = {
      val res = $.lengthCompare(n)
      if (res < 0) Smaller else if (res == 0) Equal else Larger
    }
    def lazyFoldl[B](b: B)(f: (A, B) => Option[B]): B = $.iterator.lazyFoldl(b)(f)
  }

  // In a separate class to avoid name clashes with existing distinct methods in [[Seq]] and [[Views]].
  implicit class distinctIterable[A](private val $ : Iterable[A]) extends AnyVal {
    def distinct: Iterable[A] = distinctBy(identity)
    def distinctBy[B](f: A => B): Iterable[A] = ConvertersVersionSpecific.distinctBy($, f)
  }

  def from[A](it: () => Iterator[A]): Iterable[A] = new Iterable[A] {
    override def iterator: Iterator[A] = it()
  }

  sealed trait CheckLengthResult
  case object Smaller extends CheckLengthResult
  case object Larger extends CheckLengthResult
  case object Equal extends CheckLengthResult
}
