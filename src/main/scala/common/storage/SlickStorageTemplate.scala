package common.storage

import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{ExecutionContext, Future}

import scalaz.syntax.functor.ToFunctorOps
import scalaz.OptionT
import common.rich.func.BetterFutureInstances._
import common.rich.func.ToMoreFunctorOps._

/**
 * Although [[Storage]] is key-valued, it's only an abstraction above a possibly more complex, but
 * still relational (hence Slick), DAL.
 */
abstract class SlickStorageTemplate[Key, Value](implicit ec: ExecutionContext) extends
    StorageTemplate[Key, Value] {
  protected type Profile <: JdbcProfile
  protected val profile: JdbcProfile
  protected val db: Profile#Backend#Database

  import profile.api._

  protected type Entity
  protected type EntityTable <: Table[Entity]
  protected val tableQuery: TableQuery[EntityTable]
  protected def toEntity(k: Key, v: Value): Entity
  protected def extractValue(e: Entity): Value
  override protected def internalUpdate(k: Key, v: Value): Future[_] =
    db.run(tableQuery.filter(keyFilter(k)).update(toEntity(k, v)))
  override protected def internalReplace(k: Key, v: Value): Future[_] =
    db.run(tableQuery.insertOrUpdate(toEntity(k, v)))

  protected def keyFilter(k: Key)(e: EntityTable): Rep[Boolean]
  protected def toFilter(k: Key) = tableQuery.filter(keyFilter(k))
  override protected def internalDelete(k: Key) = db.run(toFilter(k).delete)

  override def storeMultiple(kvs: Seq[(Key, Value)]) =
    db.run(tableQuery ++= kvs.map(e => toEntity(e._1, e._2))).void
  override def load(k: Key) = db.run(toFilter(k).result).toOptionTF(_.headOption map extractValue)

  override def utils = new TableUtilsTemplate() {
    override def createTable() = db run tableQuery.schema.create
    override protected def forceDropTable() = db run tableQuery.schema.drop
    override def clearTable() = db run tableQuery.delete
    override def doesTableExist =
      db run MTable.getTables map (tables => tables.exists(_.name.name == tableQuery.baseTableRow.tableName))
  }
}
