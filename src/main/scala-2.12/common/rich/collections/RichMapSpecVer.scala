package common.rich.collections

import cats.Semigroup
import cats.implicits.catsSyntaxSemigroup

private object RichMapSpecVer {
  def properMapValues[K, V, V2]($ : Map[K, V], f: V => V2): Map[K, V2] = $.mapValues(f).view.force
  def mergeIntersectingSemigroup[K, V: Semigroup]($ : Map[K, V], other: Map[K, V]): Map[K, V] =
    $.filterKeys(other.contains).map(e => (e._1, e._2 |+| other(e._1))).view.force
}
