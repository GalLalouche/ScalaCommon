package common.rich.func

import common.AuxSpecs
import common.rich.func.TuplePLenses.{tuple2First, tuple2Second}
import org.scalatest.FreeSpec

class TuplePLensesTest extends FreeSpec with AuxSpecs {
  "tuple2First" in {
    List("foo" -> "bar").map(tuple2First.modify(_.toUpperCase)) shouldReturn List("FOO" -> "bar")
  }
  "tuple2Second" in {
    List("foo" -> "bar").map(tuple2Second.modify(_.toUpperCase)) shouldReturn List("foo" -> "BAR")
  }
}
