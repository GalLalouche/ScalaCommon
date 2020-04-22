package common.rich.func

import org.scalatest.FreeSpec

import scalaz.std.option.optionInstance
import common.rich.func.MoreSetInstances._
import common.rich.func.MoreTraverseInstances._
import common.rich.func.ToTraverseMonadPlusOps._

import common.test.AuxSpecs

class ToTraverseMonadPlusOpsTest extends FreeSpec with AuxSpecs {
  "filterM" in {
    Set(1, 2, 3).filterM[Option](x => Some(x % 2 == 0)).get shouldReturn Set(2)
  }
}
