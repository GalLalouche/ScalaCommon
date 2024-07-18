package common.rich.func

import scalaz.std.MapInstances
import scalaz.Monoid

object MoreMapInstances {
  implicit def basicMonoid[K, V]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map()
    override def append(f1: Map[K, V], f2: => Map[K, V]): Map[K, V] = f1 ++ f2
  }
}
