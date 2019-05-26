package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichList._
import org.scalatest.FreeSpec

class RichListTest extends FreeSpec with AuxSpecs {
  "headTailOpt" - {
    "Nil returns None" in {
      Nil.headTailOption shouldReturn None
    }
    "Single element returns a head and an empty tail" in {
      List(1).headTailOption shouldReturn Some(1, Nil)
    }
    "Multiple elements returns a head and a tail" in {
      List(1, 2, 3).headTailOption shouldReturn Some(1, List(2, 3))
    }
  }
}
