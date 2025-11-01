package common.rich.primitives

import org.scalatest.FreeSpec

import common.rich.primitives.RichBoolean._
import common.test.AuxSpecs

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
      truthTables(
        _ ⊕ _,
        (false, false, false),
        (false, true, true),
        (true, false, true),
        (true, true, false),
      )
    }
    "implies" - {
      truthTables(
        _ ==> _,
        (false, false, true),
        (false, true, true),
        (true, false, false),
        (true, true, true),
      )
    }
    "fold" - {
      true.fold(42, ???) shouldReturn 42
      false.fold(???, 42) shouldReturn 42
    }
  }

  "or" in {
    Vector(1, 2, 3, 4, 5).filter(RichBoolean.or(_ == 3, _ % 2 == 0)) shouldReturn Vector(2, 3, 4)
  }
  "and" in {
    Vector(1, 2, 3, 4, 5).filter(RichBoolean.and(_ < 5, _ > 1)) shouldReturn Vector(2, 3, 4)
  }

  private def truthTables(
      f: (Boolean, Boolean) => Boolean,
      bbs: (Boolean, Boolean, Boolean)*,
  ): Unit = bbs.foreach(Function.tupled(truthTable(_, _, f, _)))
  private def truthTable(
      b1: Boolean,
      b2: Boolean,
      f: (Boolean, Boolean) => Boolean,
      expectedOutput: Boolean,
  ): Unit = {
    def toString(b: Boolean) = if (b) "T" else "F"
    s"${toString(b1)}${toString(b2)}" in {
      f(b1, b2) shouldBe expectedOutput
    }
  }
}
