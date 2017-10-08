package common.storage

import common.storage.TableUtils.{ClearOrCreateResult, Cleared, Created}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.FutureInstances
import scalaz.syntax.ToFunctorOps

/** Provides overrides since the TableUtils trait can't have implicit parameters. */
abstract class TableUtilsTemplate(implicit ec: ExecutionContext) extends TableUtils
    with FutureInstances with ToFunctorOps {
  /** Fails if the table already exists. */
  def createTable(): Future[_]
  /** Fails if the table doesn't exist. */
  def clearTable(): Future[_]
  /** If the table doesn't exit, create it; otherwise, clear it. */
  def clearOrCreateTable(): Future[ClearOrCreateResult] =
    doesTableExist.flatMap(if (_) clearTable() >| Cleared else createTable() >| Created)

  protected def forceDropTable(): Future[_]
  /** Returns true if the table existed before and was actually dropped. */
  def dropTable(): Future[Boolean] =
    doesTableExist.flatMap(b => if (b) forceDropTable() >| true else Future successful false)
  def doesTableExist: Future[Boolean]
}
