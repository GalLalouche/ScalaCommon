package common.rich.collections

import cats.Semigroup
import cats.implicits.catsSyntaxSemigroup

import scala.collection.mutable

private object RichMapSpecVer {
  def properMapValues[K, V, V2]($ : Map[K, V], f: V => V2): Map[K, V2] = $.mapValues(f).view.force
  def properMapValues[K, V, V2]($ : mutable.Map[K, V], f: V => V2): Map[K, V2] =
    $.mapValues(f).toMap
  def mergeIntersectingSemigroup[K, V: Semigroup]($ : Map[K, V], other: Map[K, V]): Map[K, V] =
    $.filterKeys(other.contains).map(e => (e._1, e._2 |+| other(e._1))).view.force
}
