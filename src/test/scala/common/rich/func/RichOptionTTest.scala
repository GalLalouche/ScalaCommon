package common.rich.func

import java.util.NoSuchElementException

import org.scalatest.FreeSpec

import scalaz.OptionT
import common.rich.func.RichOptionT._

import common.test.AuxSpecs

class RichOptionTTest extends FreeSpec with AuxSpecs {
  "richOptionT" - {
    "orError" - {
      "When some returns the value" in {
        OptionT.some[BoxOrMsg, Int](4).orError(???) shouldReturn Box(4)
      }
      "When none returns an error" in {
        OptionT.none[BoxOrMsg, Int].orError("foobar") shouldReturn Msg("foobar")
      }
    }
    "get" - {
      "When some returns the value" in {
        OptionT.some[ContainerOrError, Int](4).get shouldReturn Container(4)
      }
      "When none returns an error" in {
        OptionT.none[ContainerOrError, Int].get.asInstanceOf[Error]
            .getFailure shouldBe a[NoSuchElementException]
      }
    }
  }

  "conditionals" - {
    "when" - {
      "true" in {when[BoxOrMsg, Int](true)(Box(4)) shouldReturn OptionT.some(4)}
      "false" in {when[BoxOrMsg, Int](false)(???) shouldReturn OptionT.none}
    }
    "whenM" - {
      "true" in {whenM[BoxOrMsg, Int](Box(true))(Box(4)) shouldReturn OptionT.some(4)}
      "false" in {whenM[BoxOrMsg, Int](Box(false))(???) shouldReturn OptionT.none}
    }
    "unless" - {
      "true" in {unless[BoxOrMsg, Int](true)(???) shouldReturn OptionT.none}
      "false" in {unless[BoxOrMsg, Int](false)(Box(4)) shouldReturn OptionT.some(4)}
    }
    "unlessM" - {
      "true" in {unlessM[BoxOrMsg, Int](Box(true))(???) shouldReturn OptionT.none}
      "false" in {unlessM[BoxOrMsg, Int](Box(false))(Box(4)) shouldReturn OptionT.some(4)}
    }
  }
}
