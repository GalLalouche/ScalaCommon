package common.uf

import scala.annotation.tailrec

import common.rich.func.MoreIterableInstances._
import scalaz.syntax.functor._

import common.rich.collections.RichTraversableOnce.richTraversableOnce

abstract class AbstractUnionFind[A] extends UnionFind[A] {
  protected def index: Map[A, Int]
  protected def getSet(a: A): Int
  protected def getParent(i: Int): Int
  /**
   * Returns inverse path to the parent, i.e., the first element in the list is the parent and the
   * last element is a. This is needed later for path shortening.
   */
  protected def getPathToParent(a: A): List[Int] = {
    @tailrec
    def go(id: Int, result: List[Int]): List[Int] = {
      val parent = getParent(id)
      if (parent == id) parent :: result else go(parent, parent :: result)
    }
    go(index(a), Nil)
  }

  override def contains(a: A) = index.contains(a)
  override def sameSet(a1: A, a2: A): Boolean = getSet(a1) == getSet(a2)
  override def sets = index.keys.fproduct(getSet).groupBy(_._2).values.map(_.map(_._1))
  override def values = index.keys
  override def hasSingleSet = numberOfSets == 1
  protected lazy val reverseMap: Map[Int, A] = index.map(_.swap)
  assert(reverseMap.size == index.size)
  override def getRepresentative(a: A): A = reverseMap(getSet(a))
  // TODO Not too efficient, but I'm too too lazy to add inverse pointers right now.
  override def set(a: A): Set[A] = sets.filter(_.contains(a)).single.toSet
}
