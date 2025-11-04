package common.rich.collections

import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.freespec.AnyFreeSpec

import common.rich.collections.RichSeqView.richSeqView
import common.test.AuxSpecs

class RichSeqViewTest extends AnyFreeSpec with AuxSpecs {
  "lift" - {
    "Negative index" in {
      Vector(1, 2, 3).view.lift(-1) shouldReturn None
      Vector().view.lift(-1) shouldReturn None
      LazyList.iterate(1)(_ * 2).view.lift(-1) shouldReturn None
    }
    "0 index" - {
      "None on empty" in { Vector().view.lift(0) shouldReturn None }
      "Nonempty" in { Vector(42).view.lift(0).value shouldReturn 42 }
      "Infinite" in { LazyList.iterate(1)(_ * 2).view.lift(0).value shouldReturn 1 }
    }
    "non-zero index" - {
      "None on empty" in { Vector().view.lift(42) shouldReturn None }
      "None on out of bounds" in { Vector(1, 2, 3).view.lift(42) shouldReturn None }
      "Nonempty" in { Vector(41, 42, 43).view.lift(1).value shouldReturn 42 }
      "Nonempty last" in { Vector(41, 42, 43).view.lift(2).value shouldReturn 43 }
      "Infinite" in { LazyList.iterate(1)(_ * 2).view.lift(10).value shouldReturn 1024 }
    }
  }
}
