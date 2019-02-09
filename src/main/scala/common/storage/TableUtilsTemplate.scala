package common.storage

import common.storage.TableUtils.{ClearOrCreateResult, Cleared, Created}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.FutureInstances
import scalaz.syntax.{ToBindOps, ToFunctorOps}

/** Provides overrides requiring an ExecutionContext since the TableUtils trait can't have implicit parameters. */
abstract class TableUtilsTemplate(implicit ec: ExecutionContext) extends TableUtils
    with FutureInstances with ToFunctorOps with ToBindOps {
  override def clearOrCreateTable(): Future[ClearOrCreateResult] =
    doesTableExist.ifM(clearTable() >| Cleared, createTable() >| Created)
  protected def forceDropTable(): Future[_]
  override def dropTable(): Future[Boolean] =
    doesTableExist.ifM(forceDropTable() >| true, Future successful false)
}
