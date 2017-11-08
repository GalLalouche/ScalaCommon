package common.storage

import common.concurrency.Lazy
import common.rich.RichT._
import common.storage.TableUtils.ClearOrCreateResult

import scala.concurrent.{ExecutionContext, Future}

/** SQL oriented functions for handling table creation and destruction. */
trait LazyTableUtils {
  /** Fails if the table already exists. */
  def createTable(): Lazy[_]
  /** Fails if the table doesn't exist. */
  def clearTable(): Lazy[_]
  /** If the table doesn't exit, create it; otherwise, clear it. Returns true if created, false otherwise. */
  def clearOrCreateTable(): Lazy[ClearOrCreateResult]
  /** Returns true if the table existed before and was actually dropped. */
  def dropTable(): Lazy[Boolean]
  def doesTableExist: Lazy[Boolean]
}

object LazyTableUtils {
  def fromFuture(utils: TableUtils)(implicit ec: ExecutionContext): LazyTableUtils = new LazyTableUtils {
    private def toLazy[T](f: Future[T]) = Lazy fromFuture f
    override def createTable(): Lazy[_] = utils.createTable() |> toLazy
    override def clearOrCreateTable(): Lazy[ClearOrCreateResult] = utils.clearOrCreateTable() |> toLazy
    override def clearTable(): Lazy[_] = utils.clearTable() |> toLazy
    override def doesTableExist: Lazy[Boolean] = utils.doesTableExist |> toLazy
    override def dropTable(): Lazy[Boolean] = utils.dropTable() |> toLazy
  }
}
