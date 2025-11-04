package common.rich

import org.scalatest.freespec.AnyFreeSpec

import common.rich.RichTuple._
import common.test.AuxSpecs

class RichTupleTest extends AnyFreeSpec with AuxSpecs {
  "flatten" in {
    // These mostly exists to verify successful complication
    (1, (2, 3)).flatten shouldReturn (1, 2, 3)
    ((1, 2), 3).flatten shouldReturn (1, 2, 3)
  }
}
