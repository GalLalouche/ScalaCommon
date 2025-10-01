package common.storage

import cats.implicits.toFunctorOps
import slick.jdbc.JdbcProfile
import slick.jdbc.meta.MTable

import scala.concurrent.{ExecutionContext, Future}

import common.rich.func.kats.ToMoreFunctorOps.toMoreFunctorOps

/**
 * Although [[Storage]] is key-valued, it's only an abstraction above a possibly more complex, but
 * still relational (hence Slick), DAL.
 *
 * To ensure uniqueness of keys, the key column should be defined using
 * [[slick.ast.ColumnOption.PrimaryKey]]. *This is not enforced by this class*!
 */
abstract class SlickStorageTemplate[Key, Value](implicit ec: ExecutionContext)
    extends StorageTemplate[Key, Value] {
  protected type Profile <: JdbcProfile
  protected val profile: JdbcProfile
  protected val db: Profile#Backend#Database

  import profile.api._

  protected type Entity
  protected type EntityTable <: Table[Entity]
  protected val tableQuery: TableQuery[EntityTable]
  protected def toEntity(k: Key, v: Value): Entity
  protected def extractValue(e: Entity): Value
  protected override def internalUpdate(k: Key, v: Value): Future[_] =
    db.run(tableQuery.filter(keyFilter(k)).update(toEntity(k, v)))
  protected override def internalReplace(k: Key, v: Value): Future[_] =
    db.run(tableQuery.insertOrUpdate(toEntity(k, v)))

  protected def keyFilter(k: Key)(e: EntityTable): Rep[Boolean]
  protected def toFilter(k: Key) = tableQuery.filter(keyFilter(k))
  protected override def internalDelete(k: Key) = db.run(toFilter(k).delete)

  override def storeMultiple(kvs: Seq[(Key, Value)]) =
    db.run(tableQuery ++= kvs.map(Function.tupled(toEntity))).void
  override def overwriteMultipleVoid(kvs: Seq[(Key, Value)]): Future[Unit] =
    // Adapted from https://stackoverflow.com/a/35006433/73650
    db.run(
      DBIO
        .sequence(
          kvs
            .map(Function.tupled(toEntity))
            .map(tableQuery.insertOrUpdate),
        )
        .withPinnedSession,
    ).void

  override def load(k: Key) = db.run(toFilter(k).result).toOptionTF(_.headOption.map(extractValue))

  override def utils = new TableUtilsTemplate() {
    override def createTable() = db.run(tableQuery.schema.create)
    protected override def forceDropTable() = db.run(tableQuery.schema.drop)
    override def clearTable() = db.run(tableQuery.delete)
    override def doesTableExist =
      db.run(MTable.getTables)
        .map(tables => tables.exists(_.name.name == tableQuery.baseTableRow.tableName))
  }
}
