package common.storage

import org.scalatest.AsyncFreeSpec

import scala.concurrent.Future

import scalaz.syntax.bind.ToBindOpsUnapply
import common.rich.func.BetterFutureInstances._

import common.test.{AsyncAuxSpecs, BeforeAndAfterAllAsync, BeforeAndAfterEachAsync}

class SlickTableUtilsTest extends AsyncFreeSpec with AsyncAuxSpecs
    with BeforeAndAfterAllAsync with BeforeAndAfterEachAsync {
  private val table = new TestTable
  private val $: TableUtilsTemplate = table.utils

  override def afterEach(): Future[_] = $.dropTable()

  "utils" - {
    "does table exists" - {
      "no by default" in {
        $.doesTableExist shouldEventuallyReturn false
      }
      "yes after creation" in {
        $.createTable() >> $.doesTableExist shouldEventuallyReturn true
      }
      "no after drop" in {
        $.createTable() >> $.dropTable() >> $.doesTableExist shouldEventuallyReturn false
      }
      "yes after clear" in {
        $.createTable() >> $.clearTable() >> $.doesTableExist shouldEventuallyReturn true
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
        ($.createTable() >>
            $.createTableIfNotExists()).shouldEventuallyReturn(false) >>
            $.doesTableExist shouldEventuallyReturn true
      }
      "does not exist" in {
        ($.doesTableExist.shouldEventuallyReturn(false) >>
            $.createTableIfNotExists()).shouldEventuallyReturn(true) >>
            $.doesTableExist shouldEventuallyReturn true
      }
    }
    "clear" - {
      "when table throws" in {
        $.clearTable().shouldFail()
      }
      "when table exists, clears the table" in {
        $.createTable() >>
            table.store(2, "foo") >>
            table.load(2).mapValue(_ shouldReturn "foo") >>
            $.clearTable() >>
            table.load(2).shouldEventuallyReturnNone()
      }
    }
    "drop" - {
      "should fail if table does not exist" in {
        $.dropTable() shouldEventuallyReturn false
      }
      "should succeed if table exists" in {
        $.createTable() >> $.dropTable() shouldEventuallyReturn true
      }
    }
  }
}
