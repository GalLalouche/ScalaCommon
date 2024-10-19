package common.uf

import common.rich.func.MoreIteratorInstances._
import monocle.Monocle.toApplyLensOps
import monocle.syntax.fields._1
import scalaz.Scalaz.ToFunctorOpsUnapply

import common.rich.collections.RichMap.richMap

/** See [[ImmutableUnionFind]] for performance analysis. */
// TODO the mutable version
class ImmutableUnionMap[K, V](override val keySet: ImmutableUnionFind[K], map: Map[K, V])
    extends UnionMap[K, V] {
  assert(
    keySet.sets.forall(_.forall(map.contains _ compose keySet.getRepresentative)),
    "Inconsistency in representative and index map",
  )
  override type UM[E] = ImmutableUnionMap[K, E]
  override def get(k: K): Option[V] =
    if (keySet.contains(k)) Option(map(keySet.getRepresentative(k))) else None
  override def values: Iterable[V] = map.values
  private def mapMap[V2](f: Map[K, V] => Map[K, V2]) = new ImmutableUnionMap(keySet, f(map))
  override def mapValues[V2](f: V => V2): UM[V2] = mapMap(_.properMapValues(f))
  override def update(t: (K, V)): UM[V] = {
    require(keySet.contains(t._1), s"<${t._1}> is not a valid key")
    mapMap(_ + (t &|-> _1 modify keySet.getRepresentative))
  }
  override def iterator: Iterator[(K, V)] =
    keySet.values.iterator.fproduct(map.apply _ compose keySet.getRepresentative)
  override def union(k1: K, k2: K, newValue: V): ImmutableUnionMap[K, V] = {
    val newKeySet = keySet.union(k1, k2)
    new ImmutableUnionMap(newKeySet, map - k1 - k2 + (newKeySet.getRepresentative(k2) -> newValue))
  }
  /** Throws if k1 and k2 have different values. */
  override def unionUnsafe(k1: K, k2: K): ImmutableUnionMap[K, V] = {
    val v1 = apply(k1)
    val v2 = apply(k2)
    require(v1 == v2, s"<$k1> and <$k2> have different values (<$v1> and <$v2>, respectively)")
    union(k1, k2, v1)
  }
}

object ImmutableUnionMap {
  def apply[K, V](map: Map[K, V]): ImmutableUnionMap[K, V] =
    new ImmutableUnionMap[K, V](ImmutableUnionFind(map.keySet), map)
}
