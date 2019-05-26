package common.rich.collections

import common.rich.RichT._

object RichStream {
  implicit class richStream[A](private val $: Stream[A]) extends AnyVal {
    def tailOption: Option[Stream[A]] = $.tail.optFilter(_.nonEmpty)
  }
}
