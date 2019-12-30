package common.rich.collections

import org.scalatest.FreeSpec

import common.rich.collections.RichStream._
import common.test.AuxSpecs

class RichStreamTest extends FreeSpec with AuxSpecs {
  "tailOpt" - {
    "Single element returns None" in {
      Stream(1).tailOption shouldReturn None
    }
    "Empty returns None" in {
      Stream(1).tailOption shouldReturn None
    }
    "Multiple elements returns Some" in {
      Stream(1, 2, 3).tailOption shouldReturn Some(Stream(2, 3))
    }
  }
}
