package common.storage

import common.rich.func.ToMoreFoldableOps._
import scalaz.std.option.optionInstance
import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.bind._

import scala.concurrent.{ExecutionContext, Future}

/** Provides overrides since the TableUtils trait can't have implicit parameters. */
abstract class StorageTemplate[Key, Value](implicit ec: ExecutionContext) extends Storage[Key, Value] {
  /** If a previous value exists, override it. */
  protected def internalDelete(k: Key): Future[_]
  protected def internalForceStore(k: Key, v: Value): Future[_]

  override def forceStore(k: Key, v: Value): Future[Option[Value]] = load(k) `<*ByName` internalForceStore(k, v)
  override def store(k: Key, v: Value): Future[Unit] = storeMultiple(List(k -> v))
  override def mapStore(k: Key, f: Value => Value, default: => Value): Future[Option[Value]] = for {
    loadedValue <- load(k)
    orDefault = loadedValue.mapHeadOrElse(f, default)
    result <- forceStore(k, orDefault)} yield result

  override def delete(k: Key): Future[Option[Value]] = load(k) `<*ByName` internalDelete(k)
}
