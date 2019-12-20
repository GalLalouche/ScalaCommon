package common.rich.primitives

import common.AuxSpecs
import common.rich.primitives.RichOption._
import org.scalatest.FreeSpec

class RichOptionTest extends FreeSpec with AuxSpecs {
  "getOrThrow" - {
    "with exception" - {
      "when Some" in {
        lazy val unusedThrowable: Throwable = ???
        Option(1) getOrThrow unusedThrowable shouldReturn 1
      }
      "when None" in {
        an[IndexOutOfBoundsException] should be thrownBy None.getOrThrow(new IndexOutOfBoundsException)
      }
    }
    "with message" - {
      "when Some" in {
        lazy val unusedErrorMessage: String = ???
        Option(1) getOrThrow unusedErrorMessage shouldReturn 1
      }
      "when None" in {
        val e = intercept[NoSuchElementException] {None.getOrThrow("foobar")}
        e.getMessage shouldReturn "foobar"
      }
    }
  }
}
