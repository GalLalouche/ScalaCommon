package common.storage

import slick.ast.{BaseTypedType, ScalaBaseType}

import scala.concurrent.ExecutionContext

private class TestUnitTable(implicit ec: ExecutionContext)
    extends SlickSingleKeyColumnStorageTemplate[String, Unit]
    with StorageSpecs {
  import profile.api._

  protected override type Id = String
  protected implicit override def btt: BaseTypedType[String] = ScalaBaseType.stringType
  protected override type Entity = String
  protected class Rows(tag: Tag) extends Table[Entity](tag, "TABLE") {
    def key = column[String]("KEY", O.PrimaryKey)
    def * = key
  }
  protected override type EntityTable = Rows
  protected override val tableQuery = TableQuery[EntityTable]
  protected override def toEntity(s: String, u: Unit) = s
  protected override def extractId(k: String): String = k
  protected override def extractValue(e: Entity) = e
  protected override def toId(et: EntityTable) = et.key
}
