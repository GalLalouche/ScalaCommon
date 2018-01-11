package common.rich.func

import common.AuxSpecs
import org.scalatest.FreeSpec

import scalaz.std.{ListInstances, OptionInstances}

class ToTraverseMonadPlusOpsTest extends FreeSpec with AuxSpecs
    with ToTraverseMonadPlusOps with OptionInstances with ListInstances {
  "filterTraverse" in {
    List(1, 2, 3).filterTraverse[Option](x => Some(x % 2 == 0)).get shouldReturn List(2)
  }
}
