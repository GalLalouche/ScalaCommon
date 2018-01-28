package common.storage

import common.storage.TableUtils.{ClearOrCreateResult, Cleared, Created}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.FutureInstances
import scalaz.syntax.{ToBindOps, ToFunctorOps}

/** Provides overrides since the TableUtils trait can't have implicit parameters. */
abstract class TableUtilsTemplate(implicit ec: ExecutionContext) extends TableUtils
    with FutureInstances with ToFunctorOps with ToBindOps {
  /** Fails if the table already exists. */
  def createTable(): Future[_]
  /** Fails if the table doesn't exist. */
  def clearTable(): Future[_]
  /** If the table doesn't exit, create it; otherwise, clear it. */
  def clearOrCreateTable(): Future[ClearOrCreateResult] =
    doesTableExist.ifM(clearTable() >| Cleared, createTable() >| Created)

  protected def forceDropTable(): Future[_]
  /** Returns true if the table existed before and was actually dropped. */
  def dropTable(): Future[Boolean] =
    doesTableExist.ifM(forceDropTable() >| true, Future successful false)
  def doesTableExist: Future[Boolean]
}
