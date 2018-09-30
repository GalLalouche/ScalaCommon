package common.rich.primitives

import common.AuxSpecs
import org.scalatest.FreeSpec
import RichBoolean._

class RichBooleanTest extends FreeSpec with AuxSpecs {
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
