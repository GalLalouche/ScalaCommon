package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichSeq.richSeq
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class RichSeqPropertyTest extends PropSpec with PropertyChecks with AuxSpecs {
	property("shuffle is a permutation") {
		forAll { xs: Seq[Int] =>
			whenever(xs.nonEmpty) {
				xs.shuffle.sorted shouldReturn xs.sorted
			}
		}
	}

	property("sample is a subset") {
		forAll { xs: Vector[Int] =>
			val l = Random.nextInt(xs.length + 1)
			for (x <- xs sample l)
				xs.contains(x) shouldReturn true
		}
	}
}
