package common.storage

import org.scalatest.AsyncFreeSpec

import scalaz.syntax.bind.ToBindOps
import common.rich.func.BetterFutureInstances._

import common.test.{AsyncAuxSpecs, BeforeAndAfterEachAsync}

class SlickSingleKeyColumnStorageTemplateTest
    extends AsyncFreeSpec with AsyncAuxSpecs with BeforeAndAfterEachAsync {
  private val table = new TestTable
  override def beforeEach = table.utils.clearOrCreateTable()
  "storage" - {
    "store" - {
      "stores when new value" in {
        table.store(1, "foo") >> table.store(2, "bar") >> checkAll(
          table.load(1).valueShouldEventuallyReturn("foo"),
          table.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "throws when value already exists" in {
        table.store(1, "foo") >> table.store(1, "bar").shouldFail()
      }
    }
    "store multiples" - {
      "stores when new value" in {
        table.storeMultiple(Vector(1 -> "foo", 2 -> "bar")) >> checkAll(
          table.load(1).valueShouldEventuallyReturn("foo"),
          table.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "throws when value already exists" in {
        table.store(1, "foo") >> table.storeMultiple(Vector(2 -> "bar", 1 -> "moo")).shouldFail()
      }
    }
    "update" - {
      "returns None when no previous value existed" in {
        table.store(1, "foo") >> checkAll(
          table.update(2, "bar").shouldEventuallyReturnNone(),
          table.load(1).valueShouldEventuallyReturn("foo"),
          table.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "returns previous value when it existed" in {
        table.store(1, "foo") >> checkAll(
          table.load(1).valueShouldEventuallyReturn("foo"),
          table.update(1, "bar").valueShouldEventuallyReturn("foo"),
          table.load(1).valueShouldEventuallyReturn("bar"),
        )
      }
    }
    "replace" - {
      "returns None when no previous value existed" in {
        table.store(1, "foo") >> checkAll(
          table.replace(2, "bar").shouldEventuallyReturnNone(),
          table.load(1).valueShouldEventuallyReturn("foo"),
          table.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "returns previous value when it existed" in {
        table.store(1, "foo") >> checkAll(
          table.load(1).valueShouldEventuallyReturn("foo"),
          table.replace(1, "bar").valueShouldEventuallyReturn("foo"),
          table.load(1).valueShouldEventuallyReturn("bar"),
        )
      }
    }
    "delete" - {
      "does nothing and returns None when no previous value" in {
        table.store(1, "foo") >> checkAll(
          table.delete(2).shouldEventuallyReturnNone(),
          table.load(1).valueShouldEventuallyReturn("foo"),
        )
      }
      "returns previous value and deletes when it existed" in {
        table.store(1, "foo") >> table.store(2, "bar") >> checkAll(
          table.delete(2).valueShouldEventuallyReturn("bar"),
          table.load(1).valueShouldEventuallyReturn("foo"),
          table.load(2).shouldEventuallyReturnNone(),
        )
      }
    }
  }
}
