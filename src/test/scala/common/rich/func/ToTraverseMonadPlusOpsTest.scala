package common.rich.func

import common.AuxSpecs
import common.rich.func.ToTraverseMonadPlusOps._
import org.scalatest.FreeSpec
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance

class ToTraverseMonadPlusOpsTest extends FreeSpec with AuxSpecs {
  "filterTraverse" in {
    List(1, 2, 3).filterTraverse[Option](x => Some(x % 2 == 0)).get shouldReturn List(2)
  }
}
