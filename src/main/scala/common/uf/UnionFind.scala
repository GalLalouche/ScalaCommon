package common.uf

trait UnionFind[A] {
  type UF <: UnionFind[A]
  def contains(a: A): Boolean
  def sameSet(a1: A, a2: A): Boolean
  def union(a1: A, a2: A): UF
  /**
   * Technically it's a set of sets, but this is a more efficient implementation since it doesn't
   * require comparing and hashing entire sets.
   */
  def sets: Iterable[Iterable[A]]
  def values: Iterable[A]
  def numberOfSets: Int
}
