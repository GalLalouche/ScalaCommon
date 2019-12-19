package common.storage

import org.scalatest.{AsyncFreeSpec, BeforeAndAfter}
import org.scalatest.OptionValues._

import scala.concurrent.Future

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.bind.ToBindOpsUnapply

import common.rich.RichFuture._
import common.AuxSpecs

class SlickTableUtilsTest extends AsyncFreeSpec with AuxSpecs with BeforeAndAfter {
  private val table = new TestTable
  private val $ = table.utils
  private def setup(): Future[_] = $.dropTable().toTry

  "utils" - {
    "does table exists" - {
      "no by default" in {
        setup() >> $.doesTableExist.map(_ shouldReturn false)
      }
      "yes after creation" in {
        setup() >> $.createTable() >> $.doesTableExist.map(_ shouldReturn true)
      }
      "no after drop" in {
        setup() >> $.createTable() >> $.dropTable() >> $.doesTableExist.map(_ shouldReturn false)
      }
      "yes after clear" in {
        setup() >> $.createTable() >> $.clearTable() >> $.doesTableExist.map(_ shouldReturn true)
      }
    }
    "create" - {
      "should not throw when table doesn't exist" in {
        setup() >> $.createTable().shouldNotFail()
      }
      "should fail when table exists" in {
        setup() >> $.createTable() >> $.createTable().shouldFail()
      }
      "succeed after drop" in {
        setup() >> $.createTable() >> $.dropTable() >> $.createTable().shouldNotFail()
      }
    }
    "createTableIfNotExists" - {
      "exists" in {
        setup() >>
            $.createTable() >>
            $.createTableIfNotExists().map(_ shouldReturn false) >>
            $.doesTableExist.map(_ shouldReturn true)
      }
      "does not exist" in {
        setup() >>
            $.doesTableExist.map(_ shouldReturn false) >>
            $.createTableIfNotExists().map(_ shouldReturn true) >>
            $.doesTableExist.map(_ shouldReturn true)
      }
    }
    "clear" - {
      "when table throws" in {
        setup() >>
            $.clearTable().shouldFail()
      }
      "when table exists, clears the table" in {
        setup() >>
            $.createTable() >>
            table.store(2, "foo") >>
            table.load(2).map(_.value shouldReturn "foo") >>
            $.clearTable() >>
            table.load(2).map(_ shouldReturn None)
      }
    }
    "drop" - {
      "should fail if table does not exist" in {
        setup() >> $.dropTable().map(_ shouldReturn false)
      }
      "should succeed if table exists" in {
        setup() >> $.createTable() >> $.dropTable().map(_ shouldReturn true)
      }
    }
  }
}
