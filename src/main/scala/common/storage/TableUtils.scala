package common.storage

import common.storage.TableUtils.ClearOrCreateResult

import scala.concurrent.Future

/** SQL oriented functions for handling table creation and destruction. */
trait TableUtils {
  /** Fails if the table already exists. */
  def createTable(): Future[_]
  /** Fails if the table doesn't exist. */
  def clearTable(): Future[_]
  /** If the table doesn't exit, create it; otherwise, clear it. Returns true if the table was created, false if */
  def clearOrCreateTable(): Future[ClearOrCreateResult]
  /** Returns true if the table existed before and was actually dropped. */
  def dropTable(): Future[Boolean]
  def doesTableExist: Future[Boolean]
}

object TableUtils {
  sealed trait ClearOrCreateResult
  case object Cleared extends ClearOrCreateResult
  case object Created extends ClearOrCreateResult
}
