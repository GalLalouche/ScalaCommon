package common.storage

import cats.implicits.catsSyntaxFlatMapOps
import org.scalatest.freespec.AsyncFreeSpec

import scala.concurrent.Future

import common.test.{AsyncAuxSpecs, BeforeAndAfterEachAsync}

class SetStorageImplTest extends AsyncFreeSpec with AsyncAuxSpecs with BeforeAndAfterEachAsync {
  private val table: SetStorageImpl[String] = new SetStorageImpl(new TestUnitTable())
  override def beforeEach(): Future[_] = table.utils.createTable()
  override def afterEach(): Future[_] = table.utils.dropTable()

  "add" in {
    table.add("foobar") >>
      (table.contains("foobar") shouldEventuallyReturn true) >>
      (table.contains("barfoo") shouldEventuallyReturn false)
  }
  "delete" in {
    (table.delete("foobar") shouldEventuallyReturn false) >>
      table.add("foobar") >>
      (table.delete("foobar") shouldEventuallyReturn true) >>
      (table.contains("foobar") shouldEventuallyReturn false)
  }
}
