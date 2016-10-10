package common

import common.rich.RichT._

/**
 * A LazyMap is basically a memoized function.
 * That is, if the value matching the key does not exist, the function will be applied to create it.
 * This class is immutable.
 */
class LazyMap[K, V] private (f: K => V, currentMap: Map[K, V]) extends Function[K, V] {

  /**
   * Possibly update the map with the correct value of the key.
   * @param k the key to use
   * @return A tuple of the matching value, and the new map. 
   * If the key did not exist in the map, the updated map will be returned.
   * Other wise, the current map will be returned.
   */
  def update(k: K): (V, LazyMap[K, V]) = {
    val currentValue = currentMap.get(k)
    if (currentValue.isDefined)
      currentValue.get -> this
    else
      f(k).mapTo(v => v -> new LazyMap(f, currentMap + (k -> v)))
  }

  /**
   * Gets the correct value for the map. unlike [[LazyMap.update(K)]], this method
   * does not update the map. Therefore, if the map does not hold a matching value
   * for the key, it will not be memoized.
   */
  def apply(k: K): V = get(k).getOrElse(f(k))
  def get(k: K): Option[V] = currentMap.get(k)
  def size = currentMap.size
  def currentValues = currentMap.values
  def currentKeys = currentMap.keys
}

object LazyMap {
  /**
   * Creates a new lazy map
   * @param f the lazy function to apply on missing cases
   */
  def apply[K, V](f: K => V): LazyMap[K, V] = {
    require(f != null)
    new LazyMap[K, V](f, Map[K, V]())
  }
}
