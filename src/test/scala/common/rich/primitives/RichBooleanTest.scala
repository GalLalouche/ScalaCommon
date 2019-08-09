package common.rich.primitives

import org.scalatest.FreeSpec

import common.AuxSpecs
import common.rich.primitives.RichBoolean._

class RichBooleanTest extends FreeSpec with AuxSpecs {
  "richBoolean" - {
    "ifTrue" - {
      "when true should return Some of object" in {
        true.ifTrue(1) shouldReturn Option(1)
      }
      "when false should return None" in {
        false.ifTrue(???) shouldReturn None
      }
    }
    "ifFalse" - {
      "when true should return Some of object" in {
        false.ifFalse(1) shouldReturn Option(1)
      }
      "when false should return None" in {
        true.ifFalse(???) shouldReturn None
      }
    }
    "xor" - {
      "00" in {false ⊕ false shouldBe false}
      "01" in {false ⊕ true shouldBe true}
      "10" in {true ⊕ false shouldBe true}
      "11" in {true ⊕ true shouldBe false}
    }
  }

  "or" in {
    Vector(1, 2, 3, 4, 5).filter(RichBoolean.or(_ == 3, _ % 2 == 0)) shouldReturn Vector(2, 3, 4)
  }
  "and" in {
    Vector(1, 2, 3, 4, 5).filter(RichBoolean.and(_ < 5, _ > 1)) shouldReturn Vector(2, 3, 4)
  }
}
