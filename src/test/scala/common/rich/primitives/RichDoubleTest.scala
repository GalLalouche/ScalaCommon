package common.rich.primitives

import org.scalatest.freespec.AnyFreeSpec

import common.rich.primitives.RichDouble.richDouble
import common.test.AuxSpecs

class RichDoubleTest extends AnyFreeSpec with AuxSpecs {
  "isRoughly" - {
    "Greater than" in {
      (1.0 / 3.0).isRoughly(0.34) shouldReturn false
    }
    "Less than" in {
      (1.0 / 3.0).isRoughly(0.33) shouldReturn false
    }
    "Roughly" in {
      (1.0 / 3.0).isRoughly(0.333333, 0.00001) shouldReturn true
    }
  }
}
