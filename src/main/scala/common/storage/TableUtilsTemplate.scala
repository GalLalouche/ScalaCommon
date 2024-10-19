package common.storage

import scala.concurrent.{ExecutionContext, Future}

import common.rich.func.BetterFutureInstances._
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.functor.ToFunctorOps

import common.storage.TableUtils.{Cleared, ClearOrCreateResult, Created}

/**
 * Provides overrides requiring an ExecutionContext since the TableUtils trait can't have implicit
 * parameters.
 */
abstract class TableUtilsTemplate(implicit ec: ExecutionContext) extends TableUtils {
  override def createTableIfNotExists(): Future[Boolean] =
    doesTableExist.ifM(Future.successful(false), createTable() >| true)
  override def clearOrCreateTable(): Future[ClearOrCreateResult] =
    doesTableExist.ifM(clearTable() >| Cleared, createTable() >| Created)
  protected def forceDropTable(): Future[_]
  override def dropTable(): Future[Boolean] =
    doesTableExist.ifM(forceDropTable() >| true, Future.successful(false))
}
