package common.uf

import scala.language.higherKinds

trait UnionMap[K, V] extends Iterable[(K, V)] {
  type UM[E] <: UnionMap[K, E]
  def keySet: UnionFind[K]
  def get(k: K): Option[V]
  def apply(k: K): V = get(k).get
  def union(k1: K, k2: K, newValue: V): UM[V]
  /** Throws if k1 and k2 have different values. */
  def unionUnsafe(k1: K, k2: K): UM[V]
  def values: Iterable[V]
  def mapValues[V2](f: V => V2): UM[V2]
  /** Throws if the key does not exist in the map. */
  def update(t: (K, V)): UM[V]
}
