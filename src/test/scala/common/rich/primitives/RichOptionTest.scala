package common.rich.primitives

import common.AuxSpecs
import common.rich.primitives.RichOption._
import org.scalatest.FreeSpec

class RichOptionTest extends FreeSpec with AuxSpecs {
  "either" - {
    "None" in {
      None either 1 shouldReturn Left(1)
    }
    "Some" in {
      Some(1) either ??? shouldReturn Right(1)
    }
  }
}
