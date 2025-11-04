package common.rich.func

import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.TuplePLenses.{__1, __2}

import common.test.AuxSpecs

class TuplePLensesTest extends AnyFreeSpec with AuxSpecs {
  "tuple2First" in {
    Vector("foo" -> "bar").map(__1.modify(_.toUpperCase)) shouldReturn Vector(
      "FOO" -> "bar",
    )
  }
  "tuple2Second" in {
    Vector("foo" -> "bar").map(__2.modify(_.toUpperCase)) shouldReturn Vector(
      "foo" -> "BAR",
    )
  }
}
