package common.storage

import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.FutureInstances
import scalaz.syntax.ToFunctorOps

object SlickTableUtils
    extends ToFunctorOps with FutureInstances {
  trait TableProperties { // Can't a case class or something similar since db depends on driver
    val driver: JdbcProfile
    val db: driver.backend.DatabaseDef
    val table: driver.api.TableQuery[_ <: driver.api.Table[_]]
  }
  def apply(tableProperties: TableProperties)(implicit ec: ExecutionContext): TableUtilsTemplate = {
    import tableProperties.driver.api._
    val db = tableProperties.db
    val table = tableProperties.table
    new TableUtilsTemplate {
      override def createTable(): Future[_] = db run table.schema.create
      override def clearTable(): Future[_] = db run table.delete
      override def forceDropTable(): Future[_] = db run table.schema.drop
      override def doesTableExist: Future[Boolean] =
        db run MTable.getTables map (tables => tables.exists(_.name.name == table.baseTableRow.tableName))
    }
  }
}
