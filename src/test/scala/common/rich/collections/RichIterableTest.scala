package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichIterable._
import org.scalatest.FreeSpec

class RichIterableTest extends FreeSpec with AuxSpecs {
  "additional constructors" - {
    "from" in {
      val $ = RichIterable.from(Iterator(1, 2, 3, 4))
      $.toVector shouldReturn Vector(1, 2, 3, 4)
      $.toVector shouldReturn Vector(1, 2, 3, 4)
    }
    "continually" in {
      RichIterable.continually(1).take(10).toVector shouldReturn Vector.fill(10)(1)
    }
    "iterate" in {
      val $ = RichIterable.iterate(1)(_ + 1)
      $.take(10).toVector shouldReturn 1.to(10).toVector
      $.take(10).toVector shouldReturn 1.to(10).toVector
    }
  }

  "hasExactSize" - {
    "true" in {
      RichIterable.from(Iterator(1, 2, 3)).hasExactSize(3) shouldReturn true
    }
    "more" in {
      RichIterable.from(Iterator(1, 2, 3, 4)).hasExactSize(3) shouldReturn false
    }
    "less" in {
      RichIterable.from(Iterator(1, 2)).hasExactSize(3) shouldReturn false
    }
    "infinite" in {
      RichIterable.continually(1).hasExactSize(3) shouldReturn false
    }
  }
}
