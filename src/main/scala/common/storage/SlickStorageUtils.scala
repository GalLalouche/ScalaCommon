package common.storage

import common.rich.RichFuture._
import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.FutureInstances
import scalaz.syntax.ToFunctorOps

object SlickStorageUtils
    extends ToFunctorOps with FutureInstances {
  trait TableProperties {
    val driver: JdbcProfile
    val db: driver.backend.DatabaseDef
    val table: driver.api.TableQuery[_ <: driver.api.Table[_]]
  }
  def apply(tableProperties: TableProperties)(implicit ec: ExecutionContext): StorageUtils = {
    import tableProperties.driver.api._
    val db = tableProperties.db
    val table = tableProperties.table
    def toBoolean(f: Future[_]): Future[Boolean] = f.map(_ => true) orElse false
    new StorageUtils {
      override def createTable(): Future[Boolean] =
        toBoolean(db run table.schema.create)
      override def clearTable(): Future[Boolean] =
        toBoolean(db run table.delete)
      override def dropTable(): Future[Boolean] =
        toBoolean(db run table.schema.drop)
      override def doesTableExist: Future[Boolean] =
        db run MTable.getTables map (tables => tables.exists(_.name.name == table.baseTableRow.tableName))
    }
  }
}