package common

/**
 * A mutable wrapper of [[LazyMap]]. Since it is mutable, it is made thread-safe via the use of
 * (volatile & double-checked) synchronisation.
 *
 * While wrapping [[java.util.concurrent.ConcurrentHashMap]] would offer better locking performance,
 * it wouldn't guarantee a minimum number of applications of the memoized function.
 */
class CacheMap[K, V] private (@volatile private var lazyMap: LazyMap[K, V]) extends Function[K, V] {
  override def apply(k: K): V =
    lazyMap.get(k).getOrElse(synchronized(lazyMap.get(k).getOrElse(force(k))))

  /** Returns the value if it was already computed. */
  def get(k: K): Option[V] = lazyMap.get(k)

  /** Forces the re-evaluation of the function. */
  def force(k: K): V = synchronized {
    val (value, newMap) = lazyMap.update(k)
    this.lazyMap = newMap
    value
  }
}

object CacheMap {
  def apply[K, V](f: K => V) = new CacheMap[K, V](LazyMap(f))
}
