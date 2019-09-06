package common

import common.rich.RichT._

/**
 * A LazyMap is basically a memoized function. That is, if the value matching the key does not exist, the
 * function will be applied to create it.
 * This class is immutable.
 */
class LazyMap[K, V] private(f: K => V, currentMap: Map[K, V]) extends Function[K, V] {
  /**
   * Gets the value for the key, and a (possibly-updated) map with an association with the key and value.
   * If the key did not exist in the map, the updated map will be returned. Other wise, the current map will
   * be returned.
   */
  def update(k: K): (V, LazyMap[K, V]) = {
    val currentValue = currentMap.get(k)
    if (currentValue.isDefined)
      currentValue.get -> this
    else
      f(k).mapTo(v => v -> new LazyMap(f, currentMap + (k -> v)))
  }

  def apply(k: K): V = get(k).getOrElse(f(k))
  def get(k: K): Option[V] = currentMap.get(k)
  def size: Int = currentMap.size
  def currentValues: Iterable[V] = currentMap.values
  def currentKeys: Iterable[K] = currentMap.keys
}

object LazyMap {
  def apply[K, V](f: K => V): LazyMap[K, V] = {
    require(f != null)
    new LazyMap[K, V](f, Map[K, V]())
  }
}
