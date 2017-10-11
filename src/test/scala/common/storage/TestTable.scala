package common.storage

import slick.ast.{BaseTypedType, ScalaBaseType}
import slick.jdbc.H2Profile

import scala.concurrent.ExecutionContext

private class TestTable(implicit ec: ExecutionContext) extends SlickStorageTemplate[Int, String] {
  override protected val profile = H2Profile
  import profile.api._

  override implicit lazy val db: profile.backend.DatabaseDef = profile.api.Database.forURL(
    s"jdbc:h2:mem:test${System.identityHashCode(this)};DB_CLOSE_DELAY=-1", driver="org.h2.Driver")
  override protected type Id = Int
  override protected implicit def btt: BaseTypedType[Int] = ScalaBaseType.intType
  override protected type Entity = (Int, String)
  protected class Rows(tag: Tag) extends Table[Entity](tag, "TABLE") {
    def key = column[Int]("KEY", O.PrimaryKey)
    def value = column[String]("VALUE")
    def * = (key, value)
  }
  override protected type EntityTable = Rows
  override protected val tableQuery = TableQuery[EntityTable]
  override protected def toEntity(k: Int, v: String) = k -> v
  override protected def extractId(k: Int) = k
  override protected def toId(et: EntityTable) = et.key
  override protected def extractValue(e: Entity) = e._2
}
