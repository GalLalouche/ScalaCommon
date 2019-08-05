package common.uf

import common.rich.primitives.RichBoolean._

/**
 * A mutable implementation of the UnionFind (Disjoint Set) algorithm. Since it's mutable, path
 * shortening can be done on unions *and* sameSet queries, so it has true O(log*(n)) amortized performance.
 * This class is *not* thread-safe!
 */
class MutableUnionFind[A] private(parents: Array[Int], override protected val index: Map[A, Int])
    extends AbstractUnionFind[A] {
  override type UF = MutableUnionFind[A]
  private var _numberOfSets: Int = parents.length
  // Sets the parent of all the indices in the iterator to that of the head of the iterator.
  private def shorten(i: Iterator[Int]): Unit = {
    val next = i.next
    i.foreach(parents(_) = next)
  }
  override protected def getParent(i: Int): Int = parents(i)
  override protected def getSet(a: A): Int = {
    val path = getPathToParent(a)
    shorten(path.iterator)
    path.head
  }
  override def union(a1: A, a2: A): this.type = {
    if (sameSet(a1, a2).isFalse) {
      // sameSet already shortens, so there's no need to shorten again if it's the same set.
      shorten(getPathToParent(a1).iterator ++ getPathToParent(a2).iterator)
      _numberOfSets -= 1
    }
    this
  }

  override def numberOfSets: Int = _numberOfSets
}

object MutableUnionFind {
  def apply[A](a1: A, a2: A, as: A*): MutableUnionFind[A] = apply(a2 :: a1 :: as.toList)
  def apply[A](as: IterableOnce[A]): MutableUnionFind[A] = {
    val vector = as.iterator.toVector
    new MutableUnionFind[A](vector.indices.toArray, vector.zipWithIndex.toMap)
  }
}
