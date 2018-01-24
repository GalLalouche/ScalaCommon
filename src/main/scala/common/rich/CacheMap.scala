package common.rich

import common.LazyMap

/**
 * This class is mutable wrapper of [[common.LazyMap]]
 * Since it is mutable, it is made thread-safe via the use of synchronisation
 */
class CacheMap[K, V] private(private var lazyMap: LazyMap[K, V]) extends Function[K, V] {
  override def apply(k: K): V = {
    lazyMap.get(k).getOrElse {
      this.synchronized {
        lazyMap.get(k).getOrElse {
          val $ = lazyMap.update(k)
          this.lazyMap = $._2
          $._1
        }
      }
    }
  }
}

object CacheMap {
  /**
   * Creates a new cache map
   *
   * @param f the function to memoized
   */
  def apply[K, V](f: K => V) = new CacheMap[K, V](LazyMap(f))
}
