package common.uf

import common.rich.RichT._

/**
 * An immutable implementation of the UnionFind (Disjoint Set) algorithm. Since it's immutable, path
 * shortening (which actually ensures O(log*(n)) performance) can only be done on unions, but on sameSet
 * checks. In other words, this could mean that the actual performance would be O(n) in the worst case for
 * some specific union operations.
 * See MutableUnionFind for the by-the-book implementation and performance.
 */
class ImmutableUnionFind[A] private(
    parents: Vector[Int],
    override protected val index: Map[A, Int],
    override val numberOfSets: Int,
) extends AbstractUnionFind[A] {
  override type UF = ImmutableUnionFind[A]
  override protected def getSet(a: A): Int = getPathToParent(a).head
  override protected def getParent(i: Int): Int = parents(i)
  override def union(a1: A, a2: A): ImmutableUnionFind[A] = {
    val paths1 = getPathToParent(a1)
    val paths2 = getPathToParent(a2)
    // We don't early exit if a1 and a2 are already in the same set since we might apply shortening.
    val differentSet = paths1.head != paths2.head
    val destination = paths1.head
    // Shorten the paths by setting the set index of all indices in the path to the same parent.
    val shortened = paths1.iterator.++(paths2.iterator)./:(parents)(_.updated(_, destination))
    new ImmutableUnionFind[A](shortened, index, numberOfSets.mapIf(differentSet).to(_ - 1))
  }
}

object ImmutableUnionFind {
  def apply[A](a1: A, a2: A, as: A*): ImmutableUnionFind[A] = apply(a2 :: a1 :: as.toList)
  def apply[A](as: TraversableOnce[A]): ImmutableUnionFind[A] = {
    val vector = as.toVector
    new ImmutableUnionFind[A](vector.indices.toVector, vector.zipWithIndex.toMap, vector.size)
  }
}
