package common.storage

import java.time.{LocalDate, LocalDateTime}

import enumeratum.EnumEntry
import org.scalatest.AsyncFreeSpec
import slick.jdbc.JdbcType

import common.rich.func.BetterFutureInstances._
import scalaz.syntax.bind.ToBindOpsUnapply

import common.rich.RichFuture._
import common.rich.collections.RichTraversableOnce._
import common.storage.ColumnMappersTest.TestEnumeratum
import common.test.{AsyncAuxSpecs, BeforeAndAfterEachAsync}

class ColumnMappersTest
    extends AsyncFreeSpec
    with AsyncAuxSpecs
    with BeforeAndAfterEachAsync
    with StorageSpecs {
  private val cm = new ColumnMappers()
  private val cmsv = new ColumnMappersSpecVer()
  import cm._
  import cmsv._
  import profile.api._

  private implicit val enumeratumMapper: JdbcType[TestEnumeratum] =
    cm.enumeratumColumn(TestEnumeratum)
  private class Rows(tag: Tag)
      extends Table[(TestEnum, TestEnumeratum, Seq[Int], LocalDate, LocalDateTime)](tag, "table") {
    def enum = column[TestEnum]("enum")
    def enumeratum = column[TestEnumeratum]("enumeratum")
    def sequence = column[Seq[Int]]("ints")
    def localDate = column[LocalDate]("date")
    def localDateTime = column[LocalDateTime]("date_time")
    def * = (enum, enumeratum, sequence, localDate, localDateTime)
  }

  private val table = TableQuery[Rows]

  override def beforeEach() = db.run(table.schema.create).toTry >> db.run(table.delete)

  private val date = LocalDate.of(2006, 7, 8)
  private val time = LocalDateTime.of(2010, 2, 3, 10, 20, 30)
  "Can save and load" in {
    db.run(
      table += (
        TestEnum.BAZZ,
        TestEnumeratum.Quux,
        Vector(4, 8, 15, 16, 23, 42),
        date,
        time,
      ),
    ) >> db
      .run(table.result)
      .map(
        _.single shouldReturn (
          TestEnum.BAZZ,
          TestEnumeratum.Quux,
          Vector(4, 8, 15, 16, 23, 42),
          date,
          time,
        ),
      )
  }
  "Can handle empty seqs" in {
    db.run(table += (TestEnum.BAR, TestEnumeratum.Foo, Nil, date, time)) >>
      db.run(table.result)
        .map(_.single shouldReturn (TestEnum.BAR, TestEnumeratum.Foo, Nil, date, time))
  }
}

object ColumnMappersTest {
  sealed trait TestEnumeratum extends EnumEntry
  object TestEnumeratum extends enumeratum.Enum[TestEnumeratum] {
    case object Foo extends TestEnumeratum
    case object Bar extends TestEnumeratum
    case object Bazz extends TestEnumeratum
    case object Quux extends TestEnumeratum
    override def values = findValues
  }
}
