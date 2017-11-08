package common.storage

import common.concurrency.Lazy
import common.rich.RichT._

import scala.concurrent.{ExecutionContext, Future}

/** A store of key-value pairs. */
trait LazyStorage[Key, Value] {
  /** Returns the previous value associated with the key. */
  def forceStore(k: Key, v: Value): Lazy[Option[Value]]
  /** Does not override; fails on existing value. */
  def store(k: Key, v: Value): Lazy[Unit]
  /** Does not override. Returns true if *no* keys already exist in the database, false otherwise. */
  def storeMultiple(kvs: Seq[(Key, Value)]): Lazy[Unit]
  /**
   * If there is already a value for the supplied key, update it using the supplied function. Otherwise just place the
   * supplied value. Returns the previous value.
   */
  def mapStore(k: Key, f: Value => Value, default: => Value): Lazy[Option[Value]]
  /** Returns the value associated with the key. */
  def load(k: Key): Lazy[Option[Value]]
  /** Returns the value that was associated with the key. */
  def delete(k: Key): Lazy[Option[Value]]
  def utils: LazyTableUtils
}

object LazyStorage {
  def fromFuture[Key, Value](s: Storage[Key, Value])(implicit ec: ExecutionContext): LazyStorage[Key, Value] = new LazyStorage[Key, Value] {
    private def toLazy[T](f: Future[T]) = Lazy fromFuture f
    override def forceStore(k: Key, v: Value) = s.forceStore(k, v) |> toLazy
    override def store(k: Key, v: Value) = s.store(k, v) |> toLazy
    override def storeMultiple(kvs: Seq[(Key, Value)]) = s.storeMultiple(kvs) |> toLazy
    override def mapStore(k: Key, f: Value => Value, default: => Value) = s.mapStore(k, f, default) |> toLazy
    override def load(k: Key) = s.load(k) |> toLazy
    override def delete(k: Key) = s.delete(k) |> toLazy
    override def utils = LazyTableUtils.fromFuture(s.utils)
  }
}
