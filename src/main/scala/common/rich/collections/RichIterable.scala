package common.rich.collections

import common.rich.RichT._
import common.rich.primitives.RichBoolean._

object RichIterable {
  def from[A]($: => Iterator[A]): Iterable[A] = new Iterable[A] {override def iterator = $}
  def continually[A]($: => A): Iterable[A] = from(Iterator.continually($))
  def iterate[A]($: => A)(f: A => A): Iterable[A] = from(Iterator.iterate($)(f))

  implicit class richIterable[A](private val $: Iterable[A]) extends AnyVal {
    def hasExactSize(n: Int): Boolean = {
      val i = $.iterator.drop(n - 1)
      i.hasNext && {i.<|(_.next()).hasNext.isFalse}
    }
  }
}
