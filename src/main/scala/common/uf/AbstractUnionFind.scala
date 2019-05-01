package common.uf

import common.rich.func.MoreIterableInstances
import scalaz.syntax.ToFunctorOps

import scala.annotation.tailrec

abstract class AbstractUnionFind[A] extends UnionFind[A]
    with ToFunctorOps with MoreIterableInstances {
  protected def index: Map[A, Int]
  protected def getSet(a: A): Int
  protected def getParent(i: Int): Int
  /**
   * Returns inverse path to the parent, i.e., the first element in the list is the parent and the
   * last element is a. This is needed later for path shortening.
   */
  protected def getPathToParent(a: A): List[Int] = {
    @tailrec
    def aux(id: Int, result: List[Int]): List[Int] = {
      val parent = getParent(id)
      if (parent == id) parent :: result else aux(parent, parent :: result)
    }
    aux(index(a), Nil)
  }

  override def contains(a: A) = index.contains(a)
  override def sameSet(a1: A, a2: A): Boolean = getSet(a1) == getSet(a2)
  override def sets = index.keys.fproduct(getSet).groupBy(_._2).values.map(_.map(_._1))
  override def values = index.keys
}
