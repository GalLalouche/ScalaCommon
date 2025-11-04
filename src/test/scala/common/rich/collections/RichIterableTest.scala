package common.rich.collections

import org.scalatest.freespec.AnyFreeSpec

import common.rich.collections.RichIterable._
import common.test.AuxSpecs

class RichIterableTest extends AnyFreeSpec with AuxSpecs {
  "length comparisons" - {
    val $ = Vector(1, 2, 3)
    val inf = Stream.continually(5)
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
