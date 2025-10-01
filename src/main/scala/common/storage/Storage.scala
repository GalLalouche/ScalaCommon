package common.storage

import cats.Invariant
import cats.data.OptionT

import scala.concurrent.{ExecutionContext, Future}

import common.rich.func.TuplePLenses

/** A store of key-value pairs. */
trait Storage[Key, Value] {
  /**
   * Will not delete the previous value if it exists, however, it may be slower than just replacing
   * it. Returns the previous value associated with the key.
   */
  def update(k: Key, v: Value): OptionT[Future, Value]
  /**
   * *May* delete the previous value if it exists, however, it may be faster than checking if it
   * exists to begin with. Whether or not the value is actually deleted depends on the internal
   * implementation, so if you want to make sure, use delete(k) >> store(k, v). Usually, we won't
   * care if a value is deleted or not, but depending on, e.g., SQL CASCADE configurations, we
   * might. Returns the previous value associated with the key.
   */
  def replace(k: Key, v: Value): OptionT[Future, Value]
  /** Does not overwrite; fails on existing value. */
  def store(k: Key, v: Value): Future[Unit]
  /** Does not overwrite; fails if *any* key already existed in the database. */
  def storeMultiple(kvs: Seq[(Key, Value)]): Future[Unit]
  /**
   * Overwrites any existing values with the input keys. Named void right now because future
   * versions of Storage might provide a method returning overwritten values.
   */
  def overwriteMultipleVoid(kvs: Seq[(Key, Value)]): Future[Unit]
  /**
   * If there is already a value for the supplied key, update or replace it using the supplied
   * function. Otherwise just place the supplied value. Returns the previous value.
   */
  def mapStore(
      mode: StoreMode,
      k: Key,
      f: Value => Value,
      default: => Value,
  ): OptionT[Future, Value]
  /** Returns the value associated with the key. */
  def load(k: Key): OptionT[Future, Value]
  def exists(k: Key): Future[Boolean]
  /** Returns the value that was associated with the key. */
  def delete(k: Key): OptionT[Future, Value]
  def utils: TableUtils
}

object Storage {
  implicit def FunctorEv[K](implicit ec: ExecutionContext): Invariant[Storage[K, *]] =
    new Invariant[Storage[K, *]] {
      override def imap[A, B](fa: Storage[K, A])(f: A => B)(g: B => A): Storage[K, B] =
        new Storage[K, B] {
          override def update(k: K, v: B) = fa.update(k, g(v)).map(f)
          override def replace(k: K, v: B) = fa.replace(k, g(v)).map(f)
          override def store(k: K, v: B) = fa.store(k, g(v))
          override def storeMultiple(kvs: Seq[(K, B)]) =
            fa.storeMultiple(kvs.map(TuplePLenses.tuple2Second.modify(g)))
          override def overwriteMultipleVoid(kvs: Seq[(K, B)]) =
            fa.overwriteMultipleVoid(kvs.map(TuplePLenses.tuple2Second.modify(g)))
          override def mapStore(mode: StoreMode, k: K, f2: B => B, default: => B) =
            fa.mapStore(mode, k, a => g(f2(f(a))), g(default)).map(f)
          override def load(k: K) = fa.load(k).map(f)
          override def exists(k: K) = fa.exists(k)
          override def delete(k: K) = fa.delete(k).map(f)
          override def utils = fa.utils
        }
    }
}
