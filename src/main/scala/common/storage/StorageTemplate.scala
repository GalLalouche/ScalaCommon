package common.storage

import scala.concurrent.{ExecutionContext, Future}

import scalaz.syntax.bind._
import scalaz.OptionT
import common.rich.func.BetterFutureInstances._
import common.rich.func.RichOptionT._

/**
 * Provides overrides since the TableUtils trait can't have implicit parameters.
 * Like the super class, this class does not make any assumption on the specific DAL implementation, e.g.,
 * if it's relational or not. Unless you have a good reason, you probably want to implement this class
 * and not [[Storage]].
 */
abstract class StorageTemplate[Key, Value](implicit ec: ExecutionContext) extends Storage[Key, Value] {
  /** If a previous value exists, override it. */
  protected def internalDelete(k: Key): Future[_]
  protected def internalForceStore(k: Key, v: Value): Future[_]

  override def forceStore(k: Key, v: Value) =
  // Since load can return None, it's necessary to run it so the internalForceStore will be computed.
    OptionT(load(k).run `<*ByName` internalForceStore(k, v))
  override def store(k: Key, v: Value) = storeMultiple(Vector(k -> v))
  override def mapStore(k: Key, f: Value => Value, default: => Value) = for {
    value <- load(k).map(f).|(default).liftSome
    result <- forceStore(k, value)
  } yield result

  override def delete(k: Key) = load(k) `<*ByName` internalDelete(k).liftSome
  override def exists(k: Key): Future[Boolean] = load(k).isDefined
}
