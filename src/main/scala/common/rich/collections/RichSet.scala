package common.rich.collections

import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConverters._
import scala.collection.mutable

import common.rich.primitives.RichBoolean._

object RichSet {
  implicit class richSet[T](private val $ : Set[T]) extends AnyVal {
    def <=(other: Set[T]): Boolean = $.forall(other)
    def <(other: Set[T]): Boolean = <=(other) && $.size < other.size
    def >=(other: Set[T]): Boolean = new richSet(other) <= $
    def >(other: Set[T]): Boolean = new richSet(other) < $
    def isDisjointTo(other: Set[T]): Boolean = intersects(other).isFalse
    def intersects(other: Set[T]): Boolean = $.exists(other.contains)
  }

  def concurrentSet[A]: mutable.Set[A] =
    Collections.newSetFromMap(new ConcurrentHashMap[A, java.lang.Boolean]).asScala
}
