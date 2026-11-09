package common.rich.func.kats

import cats.instances.string.catsKernelStdMonoidForString
import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.kats.ToMoreEmptyOps._

import common.test.AuxSpecs

class ToMoreEmptyOpsTest extends AnyFreeSpec with AuxSpecs {
  "monoidFilter" - {
    "monoidFilterConstant" - {
      "false" in {
        "foobar".emptyFilter(false) shouldReturn ""
      }
      "true" in {
        "foobar".emptyFilter(true) shouldReturn "foobar"
      }
      "Is lazy" in {
        def nothing: String = ???
        nothing.emptyFilter(false) shouldReturn ""
      }
    }
    "monoidFilterPredicate" - {
      "false" in {
        "foobar".emptyFilter(_.length < 6) shouldReturn ""
      }
      "true" in {
        "foobar".emptyFilter(_.length == 6) shouldReturn "foobar"
      }
    }
  }
}
