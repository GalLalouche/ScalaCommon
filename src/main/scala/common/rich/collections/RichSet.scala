package common.rich.collections

import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

import common.rich.primitives.RichBoolean._

import scala.collection.JavaConverters._
import scala.collection.mutable

object RichSet {
  implicit class richSet[T](private val $: Set[T]) extends AnyVal {
    def <=[U >: T](other: Set[U]): Boolean = $ forall other
    def <(other: Set[T]): Boolean = <=(other) && $.size < other.size
    def >=(other: Set[T]): Boolean = new richSet(other) <= $
    def >(other: Set[T]): Boolean = new richSet(other) < $
    def isDisjointTo(other: Set[T]): Boolean = $.exists(other.contains).isFalse
  }

  def concurrentSet[A]: mutable.Set[A] =
    Collections.newSetFromMap(new ConcurrentHashMap[A, java.lang.Boolean]).asScala
}
