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
    "mFilterOpt" - {
      val none: OptionT[BoxOrMsg, Int] = OptionT.none
      val some: OptionT[BoxOrMsg, Int] = OptionT.some(42)
      "None remains None" in {
        none.mFilterOpt(_ => ???).run shouldReturn Box(None)
      }
      "Some" - {
        "Pred is true returns self" in {
          some.mFilterOpt(e => Box(e % 2 == 0)).run shouldReturn Box(Some(42))
        }
        "Pred is false returns None" in {
          some.mFilterOpt(e => Box(e % 2 == 1)).run shouldReturn Box(None)
        }
      }
    }
  }

  "conditionals" - {
    "when" - {
      "true" in {when(true)(BoxOrMsg(4)) shouldReturn OptionT.some(4)}
      "false" in {when(false)(???) shouldReturn OptionT.none}
    }
    "whenM" - {
      "true" in {whenM(BoxOrMsg(true))(BoxOrMsg(4)) shouldReturn OptionT.some(4)}
      "false" in {whenM(BoxOrMsg(false))(???) shouldReturn OptionT.none}
    }
    "unless" - {
      "true" in {unless(true)(???) shouldReturn OptionT.none}
      "false" in {unless(false)(BoxOrMsg(4)) shouldReturn OptionT.some(4)}
    }
    "unlessM" - {
      "true" in {unlessM(BoxOrMsg(true))(???) shouldReturn OptionT.none}
      "false" in {unlessM(BoxOrMsg(false))(BoxOrMsg(4)) shouldReturn OptionT.some(4)}
    }
  }
}
