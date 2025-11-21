package common.storage

import scala.concurrent.{ExecutionContext, Future}

class SetStorageImpl[A](internal: Storage[A, Unit])(implicit ec: ExecutionContext) {
  /** Does not overwrite; fails on existing value. */
  def add(a: A): Future[Unit] = internal.store(a)
  /** Returns true if the value existed. */
  def delete(a: A): Future[Boolean] = internal.delete(a).value.map(_.isDefined)
  def contains(a: A): Future[Boolean] = internal.exists(a)
  def utils: TableUtils = internal.utils
}
