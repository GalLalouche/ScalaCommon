package common.storage

import cats.implicits.catsSyntaxFlatMapOps
import org.scalatest.freespec.AsyncFreeSpec

import common.test.{AsyncAuxSpecs, BeforeAndAfterEachAsync}

class SlickSingleKeyColumnStorageTemplateTest
    extends AsyncFreeSpec
    with AsyncAuxSpecs
    with BeforeAndAfterEachAsync {
  private val $ = new TestTable
  override def beforeEach() = $.utils.clearOrCreateTable()
  "storage" - {
    "store" - {
      "stores when new value" in {
        $.store(1, "foo") >> $.store(2, "bar") >> checkAll(
          $.load(1).valueShouldEventuallyReturn("foo"),
          $.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "throws when value already exists" in {
        $.store(1, "foo") >> $.store(1, "bar").shouldFail()
      }
    }
    "store multiples" - {
      "stores when new value" in {
        $.storeMultiple(Vector(1 -> "foo", 2 -> "bar")) >> checkAll(
          $.load(1).valueShouldEventuallyReturn("foo"),
          $.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "throws when value already exists" in {
        $.store(1, "foo") >> $.storeMultiple(Vector(2 -> "bar", 1 -> "moo")).shouldFail()
      }
    }
    "overwriteMultipleVoid" - {
      "Overwrites existing values, add new values" in {
        $.store(1, "foo") >> $.overwriteMultipleVoid(Vector(1 -> "bar", 2 -> "bazz")) >> checkAll(
          $.load(1).valueShouldEventuallyReturn("bar"),
          $.load(2).valueShouldEventuallyReturn("bazz"),
        )
      }
    }
    "update" - {
      "returns None when no previous value existed" in {
        $.store(1, "foo") >> checkAll(
          $.update(2, "bar").shouldEventuallyReturnNone(),
          $.load(1).valueShouldEventuallyReturn("foo"),
          $.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "returns previous value when it existed" in {
        $.store(1, "foo") >> checkAll(
          $.load(1).valueShouldEventuallyReturn("foo"),
          $.update(1, "bar").valueShouldEventuallyReturn("foo"),
          $.load(1).valueShouldEventuallyReturn("bar"),
        )
      }
    }
    "replace" - {
      "returns None when no previous value existed" in {
        $.store(1, "foo") >> checkAll(
          $.replace(2, "bar").shouldEventuallyReturnNone(),
          $.load(1).valueShouldEventuallyReturn("foo"),
          $.load(2).valueShouldEventuallyReturn("bar"),
        )
      }
      "returns previous value when it existed" in {
        $.store(1, "foo") >> checkAll(
          $.load(1).valueShouldEventuallyReturn("foo"),
          $.replace(1, "bar").valueShouldEventuallyReturn("foo"),
          $.load(1).valueShouldEventuallyReturn("bar"),
        )
      }
    }
    "delete" - {
      "does nothing and returns None when no previous value" in {
        $.store(1, "foo") >> checkAll(
          $.delete(2).shouldEventuallyReturnNone(),
          $.load(1).valueShouldEventuallyReturn("foo"),
        )
      }
      "returns previous value and deletes when it existed" in {
        $.store(1, "foo") >> $.store(2, "bar") >> checkAll(
          $.delete(2).valueShouldEventuallyReturn("bar"),
          $.load(1).valueShouldEventuallyReturn("foo"),
          $.load(2).shouldEventuallyReturnNone(),
        )
      }
    }
  }
}
