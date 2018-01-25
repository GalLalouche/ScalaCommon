package common.storage

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.FutureInstances
import scalaz.syntax.{ToBindOps, ToFunctorOps}

/** Provides overrides since the TableUtils trait can't have implicit parameters. */
abstract class StorageTemplate[Key, Value](implicit ec: ExecutionContext) extends Storage[Key, Value]
    with ToFunctorOps with ToBindOps with FutureInstances {
  /** If a previous value exists, override it. */
  protected def internalDelete(k: Key): Future[_]
  protected def internalForceStore(k: Key, v: Value): Future[_]

  override def forceStore(k: Key, v: Value): Future[Option[Value]] = load(k) `<*ByName` internalForceStore(k, v)
  override def store(k: Key, v: Value): Future[Unit] = storeMultiple(List(k -> v))
  override def mapStore(k: Key, f: Value => Value, default: => Value): Future[Option[Value]] =
    load(k).flatMap(v => forceStore(k, v.fold(default)(f)))
  override def delete(k: Key): Future[Option[Value]] = load(k) `<*ByName` internalDelete(k)
}
