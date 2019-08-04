package common.rich.collections

import org.scalatest.FreeSpec

import common.AuxSpecs
import common.rich.collections.RichLazyList._

class RichLazyListTest extends FreeSpec with AuxSpecs {
  "tailOpt" - {
    "Single element returns None" in {
      LazyList(1).tailOption shouldReturn None
    }
    "Empty returns None" in {
      LazyList(1).tailOption shouldReturn None
    }
    "Multiple elements returns Some" in {
      LazyList(1, 2, 3).tailOption shouldReturn Some(LazyList(2, 3))
    }
  }
}
