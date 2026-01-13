package common.storage

import scala.concurrent.{ExecutionContext, Future}

class SetStorageImpl[A](internal: Storage[A, Unit])(implicit ec: ExecutionContext)
    extends SetStorage[A] {
  override def add(a: A): Future[Unit] = internal.store(a)
  override def delete(a: A): Future[Boolean] = internal.delete(a).value.map(_.isDefined)
  override def contains(a: A): Future[Boolean] = internal.exists(a)
  override def utils: TableUtils = internal.utils
}
