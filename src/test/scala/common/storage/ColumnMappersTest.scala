package common.storage

import org.scalatest.{AsyncFreeSpec, BeforeAndAfter}

import scala.concurrent.Future

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.bind.ToBindOpsUnapply

import common.rich.RichFuture._
import common.AuxSpecs
import common.rich.collections.RichTraversableOnce._

class ColumnMappersTest extends AsyncFreeSpec with AuxSpecs with BeforeAndAfter with StorageSpecs {
  private val cm = new ColumnMappers()
  import cm._
  import profile.api._

  private class Rows(tag: Tag) extends Table[(TestEnum, Seq[Int])](tag, "TABLE") {
    def enum = column[TestEnum]("ENUM")
    def sequence = column[Seq[Int]]("INTS")
    def * = (enum, sequence)
  }

  private val table = TableQuery[Rows]
  private def setup(): Future[_] = db.run(table.schema.create).toTry >> db.run(table.delete)

  "Can save and load" in {
    setup() >>
        db.run(table += (TestEnum.BAZZ, List(4, 8, 15, 16, 23, 42))) >>
        db.run(table.result).map(_.single shouldReturn (TestEnum.BAZZ -> List(4, 8, 15, 16, 23, 42)))
  }
  "Can handle empty lists" in {
    setup() >>
        db.run(table += (TestEnum.BAR, Nil)) >>
        db.run(table.result).map(_.single shouldReturn (TestEnum.BAR -> Nil))
  }
}
