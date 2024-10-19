package common.rich.collections

import scala.annotation.tailrec

import common.rich.RichT._

object RichStream {
  implicit class richStream[A](private val $ : Stream[A]) extends AnyVal {
    def tailOption: Option[Stream[A]] = $.tail.optFilter(_.nonEmpty)
    def compute(n: Int): Unit = {
      @tailrec def go(s: Stream[_], n: Int): Unit = if (n <= 0) () else go(s.tail, n - 1)
      go($, n - 1)
    }
  }
}
