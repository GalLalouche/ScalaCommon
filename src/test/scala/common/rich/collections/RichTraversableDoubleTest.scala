package common.rich.collections

import common.rich.collections.RichSeq.richSeq
import common.rich.collections.RichTraversableDouble._
import common.rich.primitives.RichDouble._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class RichTraversableDoubleTest extends FlatSpec with Matchers {
  "mean" should "return the mean" in {
    List(1, 1, 1, 1, 1.0).mean should be === 1
    List(1, 2, 3, 4, 5.0).mean should be === 3
    List(1, 2, 3.0, 4).mean should be === 2.5
  }

  "normalize with" should "normalize the vector" in {
    List(2, 4, 4, 4.0, 5, 5, 7, 9).normalizedByMean should be === List(-1.5, -0.5, -0.5, -0.5, 0.0, 0.0, 1.0, 2.0)
  }

  "standard deviation" should "pass the example in wikipedia" in {
    List(2, 4, 4, 4.0, 5, 5, 7, 9).standardDeviation should be === 2
  }

  "rerangeToPositives" should "return all positives" in {
    List(1, 2.0, 3).rerange2Positives.forall(_ > 0) should be === true
  }

  it should "return all positives (all negatives)" in {
    List(-1, -2.0, -3).rerange2Positives.forall(_ > 0) should be === true
  }

  it should "return all positives (all same value)" in {
    List(1, 1.0, 1).rerange2Positives.forall(_ > 0) should be === true
  }

  it should "return all positives (all zeroes)" in {
    List(0, 0.0, 0).rerange2Positives.forall(_ > 0) should be === true
  }

  it should "not change a seq of already positives" in {
    val list = List.fill(100)(new Random().nextInt(100) + 1)
    list.rerange2Positives should be === list
  }

  "normalizeByRankings" should "return the same value for two different lists with no repeats" in {
    List(1, 2, 3.0).normalizedByRankings should be === List(4, 5.0, 6).normalizedByRankings
  }

  it should "keep order" in {
    List(2, 3, 1.0).normalizedByRankings should be === List(2.0 / 3, 1.0, 1.0 / 3)
  }

  it should "not separate repeats" in {
    List(1, 2, 2.0).normalizedByRankings.toSet.size should be === 2
  }

  it should "not return the same result for sorted and unsorted data" in {
    val l = List(6, 4, 10, 10, 1, 2, 4, 900, 1, 1.0)
    l.sorted.normalizedByRankings should not be l.normalizedByRankings
  }

  it should "have 1.0 in the largest value and 0.0 in the lowest" in {
    val l1 = List(1, 2, 2, 3, 4, 5, 5, 5.0).normalizedByRankings
    l1.min should be > 0.0
    l1.max should be <= 1.0
  }

  it should "normalize by rankings" in {
    List(1.0, 2, 3).normalizedByRankings should be === List(1 / 3.0, 2.0 / 3, 1)
  }

  it should "handle repeats by average rank" in {
    List(1, 2, 2, 3.0).normalizedByRankings should be === List(0.25, 5.0 / 8, 5.0 / 8, 1)
    List(1, 2, 2, 3.0, 3, 4).normalizedByRankings should be === List(1 / 6.0, 5.0 / 12.0, 5.0 / 12, 0.75, 0.75, 1)
  }

  it should "not return 0 when there are repeats in the beginning" in {
    List(1, 1, 2, 3.0).normalizedByRankings should be === List(1.5 / 4, 1.5 / 4, 0.75, 1.0)
  }

  it should "not return 1 when there are repeats in the end" in {
    List(1, 2, 3.0, 3.0).normalizedByRankings should be === List(0.25, 0.5, 7.0 / 8, 7.0 / 8)
  }

  it should "be idempotent" in {
    val list = List.fill(100)(new Random().nextDouble)
    list.normalizedByRankings.normalizedByRankings should be === list.normalizedByRankings
  }

  "medianAbsoluteDeviation" should "work on wikipedia example" in {
    List(1, 1, 2, 2, 4, 6, 9).medianAbsoluteDeviation should be === 1
  }

  "Fixed size bins" should "throw exception on non-positive bin size" in {
    evaluating {
      List(1, 2).fixedSizeBins(0.0)
    } should produce[IllegalArgumentException]
    evaluating {
      List(1, 2).fixedSizeBins(-1.0)
    } should produce[IllegalArgumentException]
  }

  it should "work with a single entry" in {
    List(1).fixedSizeBins(2.0) should be === List(1)
  }

  it should "one entry per bin" in {
    List(0, 1, 2, 3).fixedSizeBins(1.0) should be === List(1, 1, 1, 1)
  }

  it should "repeats" in {
    List(0, 2, 4, 4, 2, 5).fixedSizeBins(1.0) should be === List(1, 0, 2, 0, 2, 1)
  }

  it should "at most one entry per bin" in {
    List(0, 2, 4, 5).fixedSizeBins(1.0) should be === List(1, 0, 1, 0, 1, 1)
  }

  it should "everything in one bin" in {
    List(10, 11, 12, 13, 14, 15, 16, 17, 18, 19).fixedSizeBins(10.0) should be === List(0, 10)
  }

  it should "complicated test" in {
    List(1, 2, 3, 4, 5, 10).fixedSizeBins(2.4) should be === List(2, 2, 1, 0, 1)
  }

  it should "shuffle" in {
    List(1, 2, 3, 4, 5, 10).shuffle.fixedSizeBins(2.4) should be === List(2, 2, 1, 0, 1)
  }

  "magnitude" should "return the size of the vector" in {
    List(1, 0, 0).magnitude should be === 1
    List(1, 2, 3).magnitude should be === Math.sqrt(14)
  }

  "unit vector" should "return a normalized vector" in {
    List.fill(100)(Random.nextInt).map(_.toDouble).unitVector.magnitude isRoughly 1 should be === true
  }

  "percentile" should "return percentile" in {
    List(1, 2, 3).percentile(0.5) should be === 2
  }
}
