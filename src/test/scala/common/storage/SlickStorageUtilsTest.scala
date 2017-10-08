package common.storage

import common.AuxSpecs
import common.rich.RichFuture._
import common.storage.SlickTableUtils.TableProperties
import org.scalatest.{BeforeAndAfter, FreeSpec}
import slick.jdbc.H2Profile
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.std.FutureInstances
import scalaz.syntax.ToBindOps

class SlickStorageUtilsTest extends FreeSpec with AuxSpecs with BeforeAndAfter
    with ToBindOps with FutureInstances {
  private val driver = H2Profile
  import driver.api._

  private val db = Database.forURL(
    s"jdbc:h2:mem:test${System.identityHashCode(this)};DB_CLOSE_DELAY=-1",
    driver = "org.h2.Driver")
  private class Rows(tag: Tag) extends Table[(String, String)](tag, "TABLE") {
    def key = column[String]("KEY", O.PrimaryKey)
    def value = column[String]("VALUE")
    def * = (key, value)
  }
  private val table = TableQuery[Rows]
  private val self = this
  private val $: TableUtils = SlickTableUtils(new TableProperties {
    override val driver = self.driver
    override val db = self.db
    override val table = self.table
  })
  before {
    $.dropTable().get
  }
  after {
    $.dropTable().get
  }
  "does table exists" - {
    "no by default" in {
      $.doesTableExist.get shouldReturn false
    }
    "yes after creation" in {
      $.createTable().>>($.doesTableExist).get shouldReturn true
    }
    "no after drop" in {
      $.createTable()
          .>>($.dropTable())
          .>>($.doesTableExist).get shouldReturn false
    }
    "yes after clear" in {
      $.createTable()
          .>>($.clearTable())
          .>>($.doesTableExist).get shouldReturn true
    }
  }
  "create" - {
    "should succeed when table doesn't exist" in {
      $.createTable().>>($.doesTableExist).get shouldReturn true
    }
    "should fail when table exists" in {
      $.createTable().get
      an[Exception] should be thrownBy $.createTable().get
    }
    "succeed after drop" in {
      $.createTable()
          .>>($.dropTable())
          .>>($.createTable()).get
    }
  }
  "clear" - {
    "clears the table if it exists" in {
      $.createTable().get
      db.run(table += "key" -> "value").get
      db.run(table.result).get.toList shouldReturn List("key" -> "value")
      $.clearTable().get
      db.run(table.result).get.toList shouldReturn Nil
    }
  }
  "drop" - {
    "should return false if table does not exist" in {
      $.dropTable().get shouldReturn false
    }
    "should succeed if table exists" in {
      $.createTable().>>($.dropTable()).get shouldReturn true
      $.doesTableExist.get shouldReturn false
    }
  }
}
