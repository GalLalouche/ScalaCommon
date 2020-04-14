package common.storage

import scala.concurrent.Future

import scalaz.OptionT

/** A store of key-value pairs. */
trait Storage[Key, Value] {
  /** Returns the previous value associated with the key. */
  def forceStore(k: Key, v: Value): OptionT[Future, Value]
  /** Does not override; fails on existing value. */
  def store(k: Key, v: Value): Future[Unit]
  /** Does not override. Returns true if *no* keys already exist in the database, false otherwise. */
  def storeMultiple(kvs: Seq[(Key, Value)]): Future[Unit]
  /**
   * If there is already a value for the supplied key, update it using the supplied function. Otherwise
   * just place the supplied value. Returns the previous value.
   */
  def mapStore(k: Key, f: Value => Value, default: => Value): OptionT[Future, Value]
  /** Returns the value associated with the key. */
  def load(k: Key): OptionT[Future, Value]
  def exists(k: Key): Future[Boolean]
  /** Returns the value that was associated with the key. */
  def delete(k: Key): OptionT[Future, Value]
  def utils: TableUtils
}
