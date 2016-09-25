package common.rich.primitives

import common.AuxSpecs
import org.scalatest.FreeSpec
import RichOption._

class RichOptionTest extends FreeSpec with AuxSpecs {
  "getOrThrow" - {
    "with exception" - {
      "when Some" in {
        lazy val t: Throwable = ???
        Option(1).getOrThrow(t) shouldReturn 1
      }
      "when None" in {
        an[IndexOutOfBoundsException] should be thrownBy None.getOrThrow(new IndexOutOfBoundsException)
      }
    }
    "with message" in {
      val caught = intercept[NoSuchElementException] {
        None.getOrThrow("foobar")
      }
      caught.getMessage shouldReturn "foobar"
    }
  }
}
