package common.storage

import org.scalatest.{AsyncFreeSpec, OneInstancePerTest}

import scala.collection.mutable
import scala.concurrent.Future

import scalaz.syntax.bind._
import common.rich.func.BetterFutureInstances._
import common.rich.func.ToTransableOps.toHoistIdOps

import common.test.AsyncAuxSpecs

class StorageTemplateTest extends AsyncFreeSpec with OneInstancePerTest with AsyncAuxSpecs {
  private val existingValues = mutable.HashMap[Int, Int]()
  private val deletedRows = mutable.Buffer[(Int, Int)]()
  private val $: Storage[Int, Int] = new StorageTemplate[Int, Int]() {
    override protected def internalDelete(k: Int) = {
      val $ = existingValues.remove(k)
      $.foreach(deletedRows += k -> _)
      Future successful $
    }
    override protected def internalUpdate(k: Int, v: Int) = {
      existingValues.put(k, v)
      Future successful Unit
    }
    override protected def internalReplace(k: Int, v: Int) =
      if (existingValues.contains(k))
        internalDelete(k) >> internalUpdate(k, v)
      else
        internalUpdate(k, v)
    override def load(k: Int) = existingValues.get(k).hoistId
    override def storeMultiple(kvs: Seq[(Int, Int)]) =
      if ((existingValues.keySet & kvs.map(_._1).toSet).nonEmpty)
        Future failed new IllegalArgumentException
      else
        overwriteMultipleVoid(kvs)
    override def overwriteMultipleVoid(kvs: Seq[(Int, Int)]) = Future successful existingValues.++=(kvs)
    override def utils = ???
  }
  "store" - {
    "has existing value throws" in {
      existingValues += 1 -> 2
      $.store(1, 4).shouldFail() >| (existingValues(1) shouldReturn 2)
    }
    "no existing value should insert the value and return true" in {
      $.store(1, 4) >| (existingValues(1) shouldReturn 4)
    }
  }
  "overwriteMultipleVoid" - {
    "Overwrites existing values, add new values" in {
      existingValues += 1 -> 2
      $.overwriteMultipleVoid(Vector(1 -> 4, 3 -> 5)) >| assertAll(Vector(
        existingValues(1) shouldReturn 4,
        existingValues(3) shouldReturn 5,
      ))
    }
    "no existing value should insert the value and return true" in {
      $.store(1, 4) >| (existingValues(1) shouldReturn 4)
    }
  }
  "update" - {
    "has existing value returns old value but does not delete" in {
      existingValues += 1 -> 2
      $.update(1, 4).valueShouldEventuallyReturn(2)
          .>|(existingValues(1).shouldReturn(4) && deletedRows.shouldBe(empty))
    }
    "has no existing value returns None" in {
      $.update(1, 4).shouldEventuallyReturnNone()
          .>|(existingValues(1).shouldReturn(4) && deletedRows.shouldBe(empty))
    }
  }
  "replace" - {
    "has existing value returns old value and deletes" in {
      existingValues += 1 -> 2
      $.replace(1, 4).valueShouldEventuallyReturn(2).>|(existingValues(1) shouldReturn 4)
          .>|(existingValues(1).shouldReturn(4) && deletedRows.shouldContainExactly(1 -> 2))
    }
    "has no existing value returns None" in {
      $.update(1, 4).shouldEventuallyReturnNone()
          .>|(existingValues(1).shouldReturn(4) && deletedRows.shouldBe(empty))
    }
  }
  "mapStore" - {
    "update" - {
      "has no existing value uses default" in {
        $.mapStore(StoreMode.Update, 1, _ => ???, 2).shouldEventuallyReturnNone()
            .>|(existingValues(1).shouldReturn(2) && deletedRows.shouldBe(empty))
      }
      "maps existing value if present but does not delete" in {
        existingValues += 1 -> 2
        $.mapStore(StoreMode.Update, 1, _ * 2, ???).valueShouldEventuallyReturn(2)
            .>|(existingValues(1).shouldReturn(4) && deletedRows.shouldBe(empty))
      }
    }
    "replace" - {
      "has no existing value uses default" in {
        $.mapStore(StoreMode.Replace, 1, _ => ???, 2).shouldEventuallyReturnNone()
            .>|(existingValues(1).shouldReturn(2) && deletedRows.shouldBe(empty))
      }
      "maps existing value if present and deletes" in {
        existingValues += 1 -> 2
        $.mapStore(StoreMode.Replace, 1, _ * 2, ???).valueShouldEventuallyReturn(2)
            .>|(existingValues(1).shouldReturn(4) && deletedRows.shouldContainExactly(1 -> 2))
      }
    }
  }
  "delete" - {
    "existing value" in {
      existingValues += 1 -> 2
      checkAll(
        $.delete(1).valueShouldEventuallyReturn(2),
        $.load(1).shouldEventuallyReturnNone(),
      )
    }
    "no existing value" in {
      checkAll(
        $.delete(1).shouldEventuallyReturnNone(),
        $.load(1).shouldEventuallyReturnNone(),
      )
    }
  }
  "exists" - {
    "true" in {
      $.store(1, 4) >> $.exists(1) shouldEventuallyReturn true
    }
    "false" in {
      $.store(1, 4) >> $.exists(2) shouldEventuallyReturn false
    }
  }

  "xmap" - {
    import scalaz.syntax.invariantFunctor.ToInvariantFunctorOps

    val $2 = $.xmap[String](_.toString, _.toInt)
    "store" - {
      "has existing value throws" in {
        existingValues += 1 -> 2
        $2.store(1, "4").shouldFail() >| (existingValues(1) shouldReturn 2)
      }
      "no existing value should insert the value and return true" in {
        $2.store(1, "4") >| (existingValues(1) shouldReturn 4)
      }
    }
    "update" - {
      "has existing value returns old value" in {
        existingValues += 1 -> 2
        $.update(1, 4).valueShouldEventuallyReturn(2) >| (existingValues(1) shouldReturn 4)
      }
      "has no existing value returns None" in {
        $.update(1, 4).shouldEventuallyReturnNone() >| (existingValues(1) shouldReturn 4)
      }
    }
    "replace" - {
      "has existing value returns old value" in {
        existingValues += 1 -> 2
        $.replace(1, 4).valueShouldEventuallyReturn(2) >| (existingValues(1) shouldReturn 4)
      }
      "has no existing value returns None" in {
        $.replace(1, 4).shouldEventuallyReturnNone() >| (existingValues(1) shouldReturn 4)
      }
    }
    "mapStore" - {
      "update" - {
        "has no existing value uses default" in {
          $.mapStore(StoreMode.Update, 1, _ => ???, 2).shouldEventuallyReturnNone() >| (existingValues(1) shouldReturn 2)
        }
        "maps existing value if present" in {
          existingValues += 1 -> 2
          $.mapStore(StoreMode.Update, 1, _ * 2, ???).valueShouldEventuallyReturn(2) >| (existingValues(1) shouldReturn 4)
        }
      }
      "replace" - {
        "has no existing value uses default" in {
          $.mapStore(StoreMode.Replace, 1, _ => ???, 2).shouldEventuallyReturnNone() >| (existingValues(1) shouldReturn 2)
        }
        "maps existing value if present" in {
          existingValues += 1 -> 2
          $.mapStore(StoreMode.Replace, 1, _ * 2, ???).valueShouldEventuallyReturn(2) >| (existingValues(1) shouldReturn 4)
        }
      }
    }
    "delete" - {
      "existing value" in {
        existingValues += 1 -> 2
        checkAll(
          $2.delete(1).valueShouldEventuallyReturn("2"),
          $2.load(1).shouldEventuallyReturnNone(),
        )
      }
      "no existing value" in {
        checkAll(
          $2.delete(1).shouldEventuallyReturnNone(),
          $2.load(1).shouldEventuallyReturnNone(),
        )
      }
    }
    "exists" - {
      "true" in {
        $2.store(1, "4") >> $2.exists(1) shouldEventuallyReturn true
      }
      "false" in {
        $2.store(1, "4") >> $2.exists(2) shouldEventuallyReturn false
      }
    }
  }
}
