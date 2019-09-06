package common

/**
 * A mutable wrapper of [[LazyMap]].
 * Since it is mutable, it is made thread-safe via the use of (double-checked) synchronisation.
 *
 * While wrapping [[java.util.concurrent.ConcurrentHashMap]] would offer better locking performance, it
 * wouldn't guarantee a minimum number of applications of the memoized function.
 */
class CacheMap[K, V] private(private var lazyMap: LazyMap[K, V]) extends Function[K, V] {
  override def apply(k: K): V = lazyMap.get(k).getOrElse {
    this.synchronized {
      lazyMap.get(k).getOrElse {
        val (value, newMap) = lazyMap.update(k)
        this.lazyMap = newMap
        value
      }
    }
  }
}

object CacheMap {
  def apply[K, V](f: K => V) = new CacheMap[K, V](LazyMap(f))
}
