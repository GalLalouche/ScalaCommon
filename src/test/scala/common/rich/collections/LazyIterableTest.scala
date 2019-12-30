package common.rich.collections

import org.scalatest.FreeSpec

import common.rich.RichT._
import common.test.AuxSpecs

class LazyIterableTest extends FreeSpec with AuxSpecs {
  "iterateOptionally" in {
    LazyIterable.iterateOptionally(1)(_.opt.map(_ + 1).filter(_ <= 10)).toVector shouldReturn 1.to(10).toVector
  }
}
