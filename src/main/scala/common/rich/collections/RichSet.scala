package common.rich.collections

import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable

import common.rich.primitives.RichBoolean._

object RichSet {
  implicit class richSet[T](private val $: Set[T]) extends AnyVal {
    def <=[U >: T](other: Set[U]): Boolean = $ forall other
    def <(other: Set[T]): Boolean = <=(other) && $.size < other.size
    def >=(other: Set[T]): Boolean = other <= $
    def >(other: Set[T]): Boolean = other < $
    def isDisjointTo(other: Set[T]): Boolean = intersects(other).isFalse
    @tailrec final def intersects(other: Set[T]): Boolean =
      if (other.size < $.size) other intersects $ else $ exists other
  }

  def concurrentSet[A]: mutable.Set[A] =
    Collections.newSetFromMap(new ConcurrentHashMap[A, java.lang.Boolean]).asScala
}
