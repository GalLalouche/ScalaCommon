package common.rich.func

import org.scalatest.FreeSpec

import scalaz.std.option.optionInstance
import scalaz.std.vector.vectorInstance
import common.rich.func.ToTraverseMonadPlusOps._

import common.test.AuxSpecs

class ToTraverseMonadPlusOpsTest extends FreeSpec with AuxSpecs {
  "filterM" - {
    "Small input" in {
      import common.rich.func.MoreSetInstances._
      import common.rich.func.MoreTraverseInstances._
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
