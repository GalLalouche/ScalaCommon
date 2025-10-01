package common.storage

import cats.implicits.{catsSyntaxIfM, toFunctorOps}

import scala.concurrent.{ExecutionContext, Future}

import common.storage.TableUtils.{Cleared, ClearOrCreateResult, Created}

/**
 * Provides overrides requiring an ExecutionContext since the TableUtils trait can't have implicit
 * parameters.
 */
abstract class TableUtilsTemplate(implicit ec: ExecutionContext) extends TableUtils {
  override def createTableIfNotExists(): Future[Boolean] =
    doesTableExist.ifM(Future.successful(false), createTable() as true)
  override def clearOrCreateTable(): Future[ClearOrCreateResult] =
    doesTableExist.ifM(clearTable() as Cleared, createTable() as Created)
  protected def forceDropTable(): Future[_]
  override def dropTable(): Future[Boolean] =
    doesTableExist.ifM(forceDropTable() as true, Future.successful(false))
}
