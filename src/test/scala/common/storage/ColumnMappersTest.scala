package common.storage

import java.time.{LocalDate, LocalDateTime}

import org.scalatest.AsyncFreeSpec

import scalaz.syntax.bind.ToBindOpsUnapply
import common.rich.func.BetterFutureInstances._

import common.rich.RichFuture._
import common.rich.collections.RichTraversableOnce._
import common.test.{AsyncAuxSpecs, BeforeAndAfterEachAsync}

class ColumnMappersTest extends AsyncFreeSpec with AsyncAuxSpecs
    with BeforeAndAfterEachAsync with StorageSpecs {
  private val cm = new ColumnMappers()
  import cm._
  import profile.api._

  private class Rows(tag: Tag) extends Table[(TestEnum, Seq[Int], LocalDate, LocalDateTime)](tag, "table") {
    def enum = column[TestEnum]("enum")
    def sequence = column[Seq[Int]]("ints")
    def localDate = column[LocalDate]("date")
    def localDateTime = column[LocalDateTime]("date_time")
    def * = (enum, sequence, localDate, localDateTime)
  }

  private val table = TableQuery[Rows]

  override def beforeEach() = db.run(table.schema.create).toTry >> db.run(table.delete)

  private val date = LocalDate.of(2006, 7, 8)
  private val time = LocalDateTime.of(2010, 2, 3, 10, 20, 30)
  "Can save and load" in {
    db.run(table += (
        TestEnum.BAZZ,
        Vector(4, 8, 15, 16, 23, 42),
        date,
        time,
    )) >> db.run(table.result).map(_.single shouldReturn(
        TestEnum.BAZZ,
        Vector(4, 8, 15, 16, 23, 42),
        date,
        time,
    ))
  }
  "Can handle empty seqs" in {
    db.run(table += (TestEnum.BAR, Nil, date, time)) >>
        db.run(table.result).map(_.single shouldReturn(TestEnum.BAR, Nil, date, time))
  }
}
