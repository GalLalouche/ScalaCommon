package common

import java.util.concurrent.ConcurrentHashMap

import common.rich.collections.RichMap.richJavaMap

/** Ensures the value for a key is computed at most once. */
class CacheMap[K, V](computation: K => V) extends Function[K, V] {
  override def apply(k: K): V = computedValues.computeIfAbsent(k, computation(_))
  def get(k: K): Option[V] = computedValues.getOpt(k)
  /** Forces the re-evaluation of the function. */
  def force(k: K): V = computedValues.compute(k, (k, _) => computation(k))

  private val computedValues: ConcurrentHashMap[K, V] = new ConcurrentHashMap()
}

object CacheMap {
  def apply[K, V](f: K => V) = new CacheMap[K, V](f)
}
