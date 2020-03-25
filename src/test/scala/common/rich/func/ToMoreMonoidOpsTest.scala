package common.rich.func

import org.scalatest.FreeSpec

import scalaz.std.string.stringInstance
import common.rich.func.ToMoreMonoidOps._

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
  "toMoreMonoidOptionOps" - {
    "getOrZero" - {
      "None returns zero" in {
        (None: Option[String]).getOrZero shouldReturn ""
      }
      "Some returns value" in {
        Some("foobar").getOrZero shouldReturn "foobar"
      }
    }
  }
}
