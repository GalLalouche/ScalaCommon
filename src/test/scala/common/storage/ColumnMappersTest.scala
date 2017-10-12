package common.storage

import common.AuxSpecs
import common.rich.RichFuture._
import common.rich.collections.RichTraversableOnce._
import org.scalatest.{BeforeAndAfter, FreeSpec}
import slick.jdbc.{H2Profile, JdbcProfile}

import scala.concurrent.ExecutionContext.Implicits.global

class ColumnMappersTest extends FreeSpec with AuxSpecs with BeforeAndAfter {
  private implicit val profile: JdbcProfile = H2Profile
  private val cm = new ColumnMappers()

  import cm._
  import profile.api._

  private val db: profile.backend.DatabaseDef = profile.api.Database.forURL(
    s"jdbc:h2:mem:test${System.identityHashCode(this)};DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
  private class Rows(tag: Tag) extends Table[(TestEnum, Seq[Int])](tag, "TABLE") {
    def enum = column[TestEnum]("ENUM")
    def sequence = column[Seq[Int]]("INTS")
    def * = (enum, sequence)
  }

  private val table = TableQuery[Rows]
  db.run(table.schema.create).get
  before {
    db.run(table.delete).get
  }

  "Can save and load" in {
    db.run(table += (TestEnum.BAZZ, List(4, 8, 15, 16, 23, 42))).get
    db.run(table.result).get.single shouldReturn(TestEnum.BAZZ, List(4, 8, 15, 16, 23, 42))
  }
  "Can handle empty lists" in {
    db.run(table += (TestEnum.BAR, Nil)).get
    db.run(table.result).get.single shouldReturn(TestEnum.BAR, Nil)
  }
}
