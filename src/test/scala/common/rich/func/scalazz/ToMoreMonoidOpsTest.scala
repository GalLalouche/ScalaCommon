package common.rich.func.scalazz

import org.scalatest.FreeSpec

import common.rich.func.scalazz.ToMoreMonoidOps.{monoidFilter, toMoreMonoidOptionOps}
import scalaz.Scalaz.stringInstance

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
