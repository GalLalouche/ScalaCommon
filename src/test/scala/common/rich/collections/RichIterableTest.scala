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

  "length comparisons" - {
    val $ = RichIterable.from(Iterator(1, 2, 3))
    val inf = RichIterable.continually(5)
    "checkLength" in {
      $.checkLength(2) shouldReturn RichIterable.Larger
      $.checkLength(3) shouldReturn RichIterable.Equal
      $.checkLength(4) shouldReturn RichIterable.Smaller
      inf.checkLength(5) shouldReturn RichIterable.Larger
    }

    "hasAtLeastSizeOf" in {
      $.hasAtLeastSizeOf(2) shouldReturn true
      $.hasAtLeastSizeOf(3) shouldReturn true
      $.hasAtLeastSizeOf(4) shouldReturn false
      inf.hasAtLeastSizeOf(5) shouldReturn true
    }

    "hasAtMostSizeOf" in {
      $.hasAtMostSizeOf(2) shouldReturn false
      $.hasAtMostSizeOf(3) shouldReturn true
      $.hasAtMostSizeOf(4) shouldReturn true
      inf.hasAtMostSizeOf(5) shouldReturn false
    }

    "isLargerThan" in {
      $.isLargerThan(2) shouldReturn true
      $.isLargerThan(3) shouldReturn false
      $.isLargerThan(4) shouldReturn false
      inf.isLargerThan(5) shouldReturn true
    }

    "isSmallerThan" in {
      $.isSmallerThan(2) shouldReturn false
      $.isSmallerThan(3) shouldReturn false
      $.isSmallerThan(4) shouldReturn true
      inf.isSmallerThan(5) shouldReturn false
    }

    "hasExactlySizeOf" in {
      $.hasExactlySizeOf(2) shouldReturn false
      $.hasExactlySizeOf(3) shouldReturn true
      $.hasExactlySizeOf(4) shouldReturn false
      inf.hasExactlySizeOf(5) shouldReturn false
    }
  }
}
