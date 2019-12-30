package common.rich.func

import org.scalatest.FreeSpec

import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import common.rich.func.ToTraverseMonadPlusOps._

import common.test.AuxSpecs

class ToTraverseMonadPlusOpsTest extends FreeSpec with AuxSpecs {
  "filterTraverse" in {
    List(1, 2, 3).filterTraverse[Option](x => Some(x % 2 == 0)).get shouldReturn List(2)
  }
}
