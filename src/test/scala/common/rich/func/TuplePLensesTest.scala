package common.rich.func

import org.scalatest.FreeSpec

import common.rich.func.TuplePLenses.{tuple2First, tuple2Second}

import common.test.AuxSpecs

class TuplePLensesTest extends FreeSpec with AuxSpecs {
  "tuple2First" in {
    List("foo" -> "bar").map(tuple2First.modify(_.toUpperCase)) shouldReturn List("FOO" -> "bar")
  }
  "tuple2Second" in {
    List("foo" -> "bar").map(tuple2Second.modify(_.toUpperCase)) shouldReturn List("foo" -> "BAR")
  }
}
