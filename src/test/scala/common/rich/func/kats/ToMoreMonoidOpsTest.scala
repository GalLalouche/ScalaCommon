package common.rich.func.kats

import cats.instances.string.catsKernelStdMonoidForString
import org.scalatest.FreeSpec

import common.rich.func.kats.ToMoreMonoidOps._

import common.test.AuxSpecs

class ToMoreMonoidOpsTest extends FreeSpec with AuxSpecs {
  "monoidFilter" - {
    "monoidFilterConstant" - {
      "false" in {
        "foobar".monoidFilter(false) shouldReturn ""
      }
      "true" in {
        "foobar".monoidFilter(true) shouldReturn "foobar"
      }
      "Is lazy" in {
        def nothing: String = ???
        nothing.monoidFilter(false) shouldReturn ""
      }
    }
    "monoidFilterPredicate" - {
      "false" in {
        "foobar".monoidFilter(_.length < 6) shouldReturn ""
      }
      "true" in {
        "foobar".monoidFilter(_.length == 6) shouldReturn "foobar"
      }
    }
  }
}
