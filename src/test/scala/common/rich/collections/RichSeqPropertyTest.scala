package common.rich.collections

import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

import scala.util.Random

import common.rich.collections.RichSeq.richSeq
import common.test.AuxSpecs

class RichSeqPropertyTest extends PropSpec with PropertyChecks with AuxSpecs {
  property("shuffle is a permutation") {
    forAll { xs: Seq[Int] =>
      whenever(xs.nonEmpty) {
        xs.shuffle.sorted shouldReturn xs.sorted
      }
    }
  }

  property("sample is a subset") {
    forAll { xs: Vector[Int] => whenever(xs.nonEmpty) {
      val l = Random.nextInt(xs.length) + 1
      val sample = xs sample l
      sample forall xs.contains
    }
    }
  }
}
