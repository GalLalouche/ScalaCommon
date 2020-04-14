package common.storage

import scala.concurrent.Future

import common.storage.TableUtils.ClearOrCreateResult

/** SQL oriented functions for handling table creation and destruction. */
trait TableUtils {
  /** Fails if the table already exists. */
  def createTable(): Future[_]
  /** Fails if the table doesn't exist. */
  def clearTable(): Future[_]
  /** Returns true if the table was created. */
  def createTableIfNotExists(): Future[Boolean]
  /** If the table doesn't exit, create it; otherwise, clear it. */
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
