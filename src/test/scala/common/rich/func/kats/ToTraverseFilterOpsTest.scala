package common.rich.func.kats

import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.kats.ToMoreTraverseFilterOps.toMoreTraverseFilterOps

import common.test.AuxSpecs

class ToTraverseFilterOpsTest extends AnyFreeSpec with AuxSpecs {
  "uniqueBy" - {
    "Small input" in {
      Vector("foo", "bar", "bazz", "quux").uniqueBy(_.length) shouldReturn Vector("foo", "bazz")
    }
    "Doesn't overflow" in {
      1.to(10000).toVector.uniqueBy(_ % 10) should have size 10
    }
  }
}
