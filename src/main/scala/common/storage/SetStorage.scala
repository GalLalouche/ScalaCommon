package common.storage

import scala.concurrent.Future

trait SetStorage[A] {
  /** Does not overwrite; fails on existing value. */
  def add(a: A): Future[Unit]
  /** Returns true if the value existed. */
  def delete(a: A): Future[Boolean]
  def contains(a: A): Future[Boolean]
  def utils: TableUtils
}
