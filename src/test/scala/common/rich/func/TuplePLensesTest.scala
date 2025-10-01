package common.rich.func

import org.scalatest.FreeSpec

import common.rich.func.TuplePLenses._

import common.test.AuxSpecs

class TuplePLensesTest extends FreeSpec with AuxSpecs {
  "tuple2First" in {
    Vector("foo" -> "bar").map(tuple2First.modify(_.toUpperCase)) shouldReturn Vector(
      "FOO" -> "bar",
    )
  }
  "tuple2Second" in {
    Vector("foo" -> "bar").map(tuple2Second.modify(_.toUpperCase)) shouldReturn Vector(
      "foo" -> "BAR",
    )
  }
}
