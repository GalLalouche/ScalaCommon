package common.storage

import org.scalatest.AsyncFreeSpec

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.bind.ToBindOps

import common.test.{AsyncAuxSpecs, BeforeAndAfterEachAsync}

class SlickStorageTemplateTest extends AsyncFreeSpec with AsyncAuxSpecs with BeforeAndAfterEachAsync {
  private val table = new TestTable
  override def beforeEach = table.utils.clearOrCreateTable()
  "storage" - {
    "store" - {
      "stores when new value" in {
        table.store(1, "foo") >> table.store(2, "bar") >> checkAll(
          table.load(1).mapValue(_ shouldReturn "foo"),
          table.load(2).mapValue(_ shouldReturn "bar"),
        )
      }
      "throws when value already exists" in {
        table.store(1, "foo") >> table.store(1, "bar").shouldFail()
      }
    }
    "store multiples" - {
      "stores when new value" in {
        table.storeMultiple(List((1, "foo"), (2, "bar"))) >> checkAll(
          table.load(1).mapValue(_ shouldReturn "foo"),
          table.load(2).mapValue(_ shouldReturn "bar"),
        )
      }
      "throws when value already exists" in {
        table.store(1, "foo") >> table.storeMultiple(List((2, "bar"), (1, "moo"))).shouldFail()
      }
    }
    "forceStore" - {
      "returns None when no previous value existed" in {
        table.store(1, "foo") >> checkAll(
            table.forceStore(2, "bar").shouldEventuallyReturnNone(),
            table.load(1).mapValue(_ shouldReturn "foo"),
            table.load(2).mapValue(_ shouldReturn "bar"),
        )
      }
      "returns previous value when it existed" in {
        table.store(1, "foo") >> checkAll(
          table.load(1).mapValue(_ shouldReturn "foo"),
          table.forceStore(1, "bar").mapValue(_ shouldReturn "foo"),
          table.load(1).mapValue(_ shouldReturn "bar"),
        )
      }
    }
    "delete" - {
      "does nothing and returns None when no previous value" in {
        table.store(1, "foo") >> checkAll(
          table.delete(2).shouldEventuallyReturnNone(),
          table.load(1).mapValue(_ shouldReturn "foo"),
        )
      }
      "returns previous value and deletes when it existed" in {
        table.store(1, "foo") >> table.store(2, "bar") >> checkAll(
          table.delete(2).mapValue(_ shouldReturn "bar"),
          table.load(1).mapValue(_ shouldReturn "foo"),
          table.load(2).shouldEventuallyReturnNone(),
        )
      }
    }
  }
}
