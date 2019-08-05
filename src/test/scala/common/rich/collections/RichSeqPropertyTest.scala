package common.rich.collections

import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

import common.AuxSpecs
import common.rich.collections.RichSeq.richSeq

class RichSeqPropertyTest extends PropSpec with ScalaCheckPropertyChecks with AuxSpecs {
  property("shuffle is a permutation") {
    forAll {xs: Seq[Int] =>
      whenever(xs.nonEmpty) {
        xs.shuffle.sorted shouldReturn xs.sorted
      }
    }
  }

  property("sample is a subset") {
    forAll {xs: Vector[Int] =>
      whenever(xs.nonEmpty) {
        val l = Random.nextInt(xs.length) + 1
        val sample = xs sample l
        sample forall xs.contains
      }
    }
  }
}
