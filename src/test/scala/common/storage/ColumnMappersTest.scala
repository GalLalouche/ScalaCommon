package common.storage

import org.scalatest.{BeforeAndAfter, FreeSpec}

import scala.concurrent.ExecutionContext.Implicits.global

import common.AuxSpecs
import common.rich.RichFuture._
import common.rich.collections.RichTraversableOnce._

class ColumnMappersTest extends FreeSpec with AuxSpecs with BeforeAndAfter with StorageSpecs {
  private val cm = new ColumnMappers()
  import cm._
  import profile.api._

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
