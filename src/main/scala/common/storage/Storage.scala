package common.storage

import scala.concurrent.Future

/** A store of key-value pairs. */
trait Storage[Key, Value] {
  /** Returns the previous value associated with the key. */
  def forceStore(k: Key, v: Value): Future[Option[Value]]
  /** Does not override; fails on existing value. */
  def store(k: Key, v: Value): Future[Unit]
  /** Does not override. Returns true if *no* keys already exist in the database, false otherwise. */
  def storeMultiple(kvs: Seq[(Key, Value)]): Future[Unit]
  /**
   * If there is already a value for the supplied key, update it using the supplied function. Otherwise
   * just place the supplied value. Returns the previous value.
   */
  def mapStore(k: Key, f: Value => Value, default: => Value): Future[Option[Value]]
  /** Returns the value associated with the key. */
  def load(k: Key): Future[Option[Value]]
  /** Returns the value that was associated with the key. */
  def delete(k: Key): Future[Option[Value]]
  def utils: TableUtils
}
