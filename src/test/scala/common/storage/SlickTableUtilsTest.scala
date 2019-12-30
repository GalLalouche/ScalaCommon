package common.storage

import org.scalatest.AsyncFreeSpec
import org.scalatest.OptionValues._

import scala.concurrent.Future

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.bind.ToBindOpsUnapply

import common.test.{AsyncAuxSpecs, BeforeAndAfterAllAsync, BeforeAndAfterEachAsync}

class SlickTableUtilsTest extends AsyncFreeSpec with AsyncAuxSpecs
    with BeforeAndAfterAllAsync with BeforeAndAfterEachAsync {
  private val table = new TestTable
  private val $: TableUtilsTemplate = table.utils

  override def afterEach(): Future[_] = $.dropTable()

  "utils" - {
    "does table exists" - {
      "no by default" in {
        $.doesTableExist.map(_ shouldReturn false)
      }
      "yes after creation" in {
        $.createTable() >> $.doesTableExist.map(_ shouldReturn true)
      }
      "no after drop" in {
        $.createTable() >> $.dropTable() >> $.doesTableExist.map(_ shouldReturn false)
      }
      "yes after clear" in {
        $.createTable() >> $.clearTable() >> $.doesTableExist.map(_ shouldReturn true)
      }
    }
    "create" - {
      "should not throw when table doesn't exist" in {
        $.createTable().shouldNotFail()
      }
      "should fail when table exists" in {
        $.createTable() >> $.createTable().shouldFail()
      }
      "succeed after drop" in {
        $.createTable() >> $.dropTable() >> $.createTable().shouldNotFail()
      }
    }
    "createTableIfNotExists" - {
      "exists" in {
        $.createTable() >>
            $.createTableIfNotExists().map(_ shouldReturn false) >>
            $.doesTableExist.map(_ shouldReturn true)
      }
      "does not exist" in {
        $.doesTableExist.map(_ shouldReturn false) >>
            $.createTableIfNotExists().map(_ shouldReturn true) >>
            $.doesTableExist.map(_ shouldReturn true)
      }
    }
    "clear" - {
      "when table throws" in {
        $.clearTable().shouldFail()
      }
      "when table exists, clears the table" in {
        $.createTable() >>
            table.store(2, "foo") >>
            table.load(2).map(_.value shouldReturn "foo") >>
            $.clearTable() >>
            table.load(2).map(_ shouldReturn None)
      }
    }
    "drop" - {
      "should fail if table does not exist" in {
        $.dropTable().map(_ shouldReturn false)
      }
      "should succeed if table exists" in {
        $.createTable() >> $.dropTable().map(_ shouldReturn true)
      }
    }
  }
}
