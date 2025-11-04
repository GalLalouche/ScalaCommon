package common

import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

import common.test.AuxSpecs

class PercentageTest extends AnyFreeSpec with AuxSpecs {
  "constructor throws on values out of bounds" in {
    an[IllegalArgumentException] shouldBe thrownBy(Percentage(-1e-5))
    an[IllegalArgumentException] shouldBe thrownBy(Percentage(1 + 1e-5))
  }
  "conform" in {
    Percentage.conform(-1e-5).p shouldReturn 0
    Percentage.conform(1 + 1e-5).p shouldReturn 1
  }
  "apply ratio" in {
    Percentage(1, 2).p shouldReturn 0.5
  }
  "*" in {
    Percentage(0.23) * 100 shouldReturn 23
    Percentage(0.23) * 100L shouldReturn 23L
    Percentage(0.25) * 0.75 shouldReturn 0.25 * 0.75
    Percentage(0.25) * 0.75f shouldReturn 0.25 * 0.75
  }
  "step" in {
    Percentage.step(0.1).map(_ * 10) shouldReturn 0.to(10)
  }
  "comparisons" - {
    "ordering" in {
      Vector[Percentage](0.5, 0.25, 0.75).sorted shouldReturn Vector(0.25, 0.5, 0.75)
    }
    ">=" in {
      Percentage(0.5) >= 0.25 shouldReturn true
      Percentage(0.5) >= 0.75 shouldReturn false
    }
    "<=" in {
      Percentage(0.5) <= 0.25 shouldReturn false
      Percentage(0.5) <= 0.75 shouldReturn true
    }
  }

  "random" in {
    Percentage(1.0).roll(new Random(0)) shouldReturn true
    Percentage(0.0).roll(new Random(0)) shouldReturn false
  }

  "prettyPrint" - {
    "No digits" in {
      Percentage(0.5).prettyPrint(0) shouldReturn "50%"
    }
    "Some digits" in {
      Percentage(0.375).prettyPrint(2) shouldReturn "37.50%"
    }
  }
}
