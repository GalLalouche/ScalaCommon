package common.rich.collections

import common.rich.RichT._

object RichLazyList {
  implicit class richLazyList[A](private val $: LazyList[A]) extends AnyVal {
    def tailOption: Option[LazyList[A]] = $.tail.optFilter(_.nonEmpty)
  }
}
