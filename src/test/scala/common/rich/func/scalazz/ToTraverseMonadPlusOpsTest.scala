package common.rich.func.scalazz

import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.scalazz.ToTraverseMonadPlusOps._
import scalaz.std.option.optionInstance
import scalaz.std.vector.vectorInstance

import common.test.AuxSpecs

class ToTraverseMonadPlusOpsTest extends AnyFreeSpec with AuxSpecs {
  "filterM" - {
    "Small input" in {
      import common.rich.func.scalazz.BetterSetInstances._
      import common.rich.func.scalazz.MoreTraverseInstances._
      Set(1, 2, 3).filterM[Option](x => Some(x % 2 == 0)).get shouldReturn Set(2)
    }
    "Doesn't overflow" in {
      1.to(10000).toVector.filterM[Option](x => Some(x % 2 == 0)).get should have size 5000
    }
  }

  "uniqueBy" - {
    "Small input" in {
      Vector("foo", "bar", "bazz", "quux").uniqueBy(_.length) shouldReturn Vector("foo", "bazz")
    }
    "Doesn't overflow" in {
      1.to(1000).toVector.uniqueBy(_ % 10) should have size 10
    }
  }
}
