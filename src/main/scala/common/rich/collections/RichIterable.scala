package common.rich.collections

import common.rich.RichT._
import common.rich.primitives.RichBoolean._

object RichIterable {
  implicit class richIterable[A](private val $: Iterable[A]) extends AnyVal {
    // Same method as in Seq, used by infinite Iterables
    // This method doesn't exist in RichIterator (or RichTraversableOnce) since checking single-time
    // consumable length consumes it, i.e., it has side-effects.
    def lengthCompare(n: Int): Int = {
      val i = $.iterator.drop(n - 1)
      if (i.hasNext.isFalse) -1 else if (i.<|(_.next()).hasNext) 1 else 0
    }

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
  }

  sealed trait CheckLengthResult
  case object Smaller extends CheckLengthResult
  case object Larger extends CheckLengthResult
  case object Equal extends CheckLengthResult
}
