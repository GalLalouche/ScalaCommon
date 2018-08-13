package common.rich.collections

import common.rich.primitives.RichBoolean._

object RichSet {
  implicit class richSet[T](private val $: Set[T]) extends AnyVal {
    def <=[U >: T](other: Set[U]): Boolean = $ forall other
    def <(other: Set[T]): Boolean = <=(other) && $.size < other.size
    def >=(other: Set[T]): Boolean = new richSet(other) <= $
    def >(other: Set[T]): Boolean = new richSet(other) < $
    def isDisjointTo(other: Set[T]): Boolean = $.exists(other.contains).isFalse
  }
}
