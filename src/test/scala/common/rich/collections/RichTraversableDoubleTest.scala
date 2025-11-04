package common.rich.collections

import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

import common.rich.collections.RichSeq.richSeq
import common.rich.collections.RichTraversableDouble._
import common.test.AuxSpecs

class RichTraversableDoubleTest extends AnyFreeSpec with AuxSpecs {
  "mean returns the mean" in {
    Vector(1, 1, 1, 1, 1).mean shouldReturn 1
    Vector(1, 2, 3, 4, 5).mean shouldReturn 3
    Vector(1, 2, 3, 4).mean shouldReturn 2.5
  }

  "normalizeByMean normalizes the vector" in {
    Vector(2, 4, 4, 4, 5, 5, 7, 9).normalizedByMean shouldReturn Vector(-1.5, -0.5, -0.5, -0.5, 0.0,
      0.0, 1.0, 2.0)
  }

  "standard deviation passes the example in wikipedia" in {
    Vector(2, 4, 4, 4, 5, 5, 7, 9).standardDeviation shouldReturn 2
  }

  "rerangeToPositives" - {
    "returns all positives" in {
      Vector(1, 2, 3).rerange2Positives.forall(_ > 0) shouldReturn true
    }

    "returns all positives (all negatives)" in {
      Vector(-1, -2, -3).rerange2Positives.forall(_ > 0) shouldReturn true
    }

    "returns all positives (all same value)" in {
      Vector(1, 1, 1).rerange2Positives.forall(_ > 0) shouldReturn true
    }

    "returns all positives (all zeroes)" in {
      Vector(0, 0, 0).rerange2Positives.forall(_ > 0) shouldReturn true
    }

    "doesn't change a seq of already positives" in {
      val list = Vector.fill(100)(new Random().nextInt(100) + 1)
      list.rerange2Positives.map(_.toInt) shouldReturn list
    }
  }

  "normalizeByRankings" - {
    "returns the same value for two different lists with no repeats" in {
      Vector(1, 2, 3).normalizedByRankings shouldReturn Vector(4, 5, 6).normalizedByRankings
    }

    "keeps order" in {
      Vector(2, 3, 1).normalizedByRankings shouldReturn Vector(2.0 / 3, 1.0, 1.0 / 3)
    }

    "doesn't separate repeats" in {
      Vector(1, 2, 2).normalizedByRankings.toSet.size shouldReturn 2
    }

    "doesn't return the same result for sorted and unsorted data" in {
      val l = Vector(6, 4, 10, 10, 1, 2, 4, 900, 1, 1)
      l.sorted.normalizedByRankings should not be l.normalizedByRankings
    }

    "has 1.0 in the largest value and 0.0 in the lowest" in {
      val l1 = Vector(1, 2, 2, 3, 4, 5, 5, 5).normalizedByRankings
      l1.min should be > 0.0
      l1.max should be <= 1.0
    }

    "normalizes by rankings" in {
      Vector(1, 2, 3).normalizedByRankings shouldReturn Vector(1 / 3.0, 2.0 / 3, 1.0)
    }

    "handles repeats by averaging rank" in {
      Vector(1, 2, 2, 3).normalizedByRankings shouldReturn Vector(0.25, 5.0 / 8, 5.0 / 8, 1)
      Vector(1, 2, 2, 3, 3, 4).normalizedByRankings shouldReturn Vector(
        1 / 6.0,
        5.0 / 12.0,
        5.0 / 12,
        0.75,
        0.75,
        1,
      )
    }

    "doesn't return 0 when there are repeats in the beginning" in {
      Vector(1, 1, 2, 3).normalizedByRankings shouldReturn Vector(1.5 / 4, 1.5 / 4, 0.75, 1.0)
    }

    "doesn't return 1 when there are repeats in the end" in {
      Vector(1, 2, 3, 3).normalizedByRankings shouldReturn Vector(0.25, 0.5, 7.0 / 8, 7.0 / 8)
    }

    "is idempotent" in {
      val list = Vector.fill(100)(new Random().nextDouble)
      list.normalizedByRankings.normalizedByRankings shouldReturn list.normalizedByRankings
    }
  }

  "medianAbsoluteDeviation works on wikipedia example" in {
    Vector(1, 1, 2, 2, 4, 6, 9).medianAbsoluteDeviation shouldReturn 1
  }

  "Fixed size bins" - {
    "throws exception on non-positive bin size" in {
      an[IllegalArgumentException] should be thrownBy Vector(1, 2).fixedSizeBins(0.0)
      an[IllegalArgumentException] should be thrownBy Vector(1, 2).fixedSizeBins(-1.0)
    }

    "works with a single entry" in {
      Vector(1).fixedSizeBins(2.0) shouldReturn Vector(1)
    }

    "one entry per bin" in {
      Vector(0, 1, 2, 3).fixedSizeBins(1.0) shouldReturn Vector(1, 1, 1, 1)
    }

    "repeats" in {
      Vector(0, 2, 4, 4, 2, 5).fixedSizeBins(1.0) shouldReturn Vector(1, 0, 2, 0, 2, 1)
    }

    "at most one entry per bin" in {
      Vector(0, 2, 4, 5).fixedSizeBins(1.0) shouldReturn Vector(1, 0, 1, 0, 1, 1)
    }

    "everything in one bin" in {
      Vector(10, 11, 12, 13, 14, 15, 16, 17, 18, 19).fixedSizeBins(10.0) shouldReturn Vector(0, 10)
    }

    "complicated test" in {
      Vector(1, 2, 3, 4, 5, 10).fixedSizeBins(2.4) shouldReturn Vector(2, 2, 1, 0, 1)
    }

    "shuffles" in {
      Vector(1, 2, 3, 4, 5, 10).shuffle.fixedSizeBins(2.4) shouldReturn Vector(2, 2, 1, 0, 1)
    }
  }
}
