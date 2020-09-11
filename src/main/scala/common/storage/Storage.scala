package common.storage

import scala.concurrent.{ExecutionContext, Future}

import scalaz.{InvariantFunctor, OptionT}
import common.rich.func.TuplePLenses

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

object Storage {
  implicit def FunctorEv[K](implicit ec: ExecutionContext): InvariantFunctor[Storage[K, *]] =
    new InvariantFunctor[Storage[K, *]] {
      import common.rich.func.BetterFutureInstances._

      override def xmap[A, B](ma: Storage[K, A], f: A => B, g: B => A) =
        new Storage[K, B] {
          override def forceStore(k: K, v: B) = ma.forceStore(k, g(v)).map(f)
          override def store(k: K, v: B) = ma.store(k, g(v))
          override def storeMultiple(kvs: Seq[(K, B)]) =
            ma.storeMultiple(kvs.map(TuplePLenses.tuple2Second.modify(g)))
          override def mapStore(k: K, f2: B => B, default: => B) =
            ma.mapStore(k, a => g(f2(f(a))), g(default)).map(f)
          override def load(k: K) = ma.load(k).map(f)
          override def exists(k: K) = ma.exists(k)
          override def delete(k: K) = ma.delete(k).map(f)
          override def utils = ma.utils
        }
    }
}