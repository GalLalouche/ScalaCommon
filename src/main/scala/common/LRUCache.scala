package common

/** 
 * An immutable LRU Cache implementation 
 * @param maxSize the maximum size of the cache.
 */
class LRUCache[K, V](maxSize: Int) extends collection.mutable.Map[K, V] {
  require(maxSize > 0)
  private val keyToIndex = collection.mutable.LinkedHashMap[K, V]()

  private def verifyConsistency() {
    assert(keyToIndex.size == size)
    assert(keyToIndex.size <= maxSize, s"size was ${keyToIndex.size} when maxSize is $maxSize")
  }

  private def checkMemory() {
    if (keyToIndex.size > maxSize)
      this -= keyToIndex.head._1
    verifyConsistency
  }

  override def +=(kv: (K, V)) = {
    keyToIndex(kv._1) = kv._2
    checkMemory
    this
  }
  override def -=(key: K) = {
    keyToIndex.remove(key)
    verifyConsistency
    this
  }
  override def iterator = keyToIndex.iterator
  override def get(key: K) = keyToIndex.get(key)

  override def contains(key: K) = {
    if (keyToIndex contains key) {
      val value = this(key)
      this -= key
      this(key) = value
      true
    } else
      false
  }
}
