package common.storage

import slick.ast.BaseTypedType
import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{ExecutionContext, Future}

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.functor.ToFunctorOps

abstract class SlickStorageTemplate[Key, Value](implicit ec: ExecutionContext) extends
    StorageTemplate[Key, Value] {
  protected type Profile <: JdbcProfile
  protected val profile: JdbcProfile
  protected val db: Profile#Backend#Database

  import profile.api._

  protected type Id
  protected implicit def btt: BaseTypedType[Id]
  protected type Entity
  protected type EntityTable <: Table[Entity]
  protected val tableQuery: TableQuery[EntityTable]
  protected def toEntity(k: Key, v: Value): Entity
  protected def extractId(k: Key): Id
  protected def toId(et: EntityTable): Rep[Id]
  protected def extractValue(e: Entity): Value
  /** If a previous value exists, override it. */
  protected def internalForceStore(k: Key, v: Value): Future[_] =
    db.run(tableQuery.insertOrUpdate(toEntity(k, v)))
  protected def internalDelete(k: Key): Future[_] =
    db.run(tableQuery.filter(toId(_) === extractId(k)).delete)

  override def storeMultiple(kvs: Seq[(Key, Value)]) =
    db.run(tableQuery ++= kvs.map(e => toEntity(e._1, e._2))).void
  override def load(k: Key) =
    db.run(tableQuery.filter(toId(_) === extractId(k)).result).map(_.headOption map extractValue)

  override def utils = new TableUtilsTemplate() {
    override def createTable(): Future[_] = db run tableQuery.schema.create
    override protected def forceDropTable() = db run tableQuery.schema.drop
    override def clearTable(): Future[_] = db run tableQuery.delete
    override def doesTableExist: Future[Boolean] =
      db run MTable.getTables map (tables => tables.exists(_.name.name == tableQuery.baseTableRow.tableName))
  }
}
