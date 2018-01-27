package common.rich.primitives

import common.AuxSpecs
import org.scalatest.FreeSpec
import RichOption._

class RichOptionTest extends FreeSpec with AuxSpecs {
  private lazy val unusedThrowable: Throwable = ???
  "getOrThrow" - {
    "with exception" - {
      "when Some" in {
        Option(1) getOrThrow unusedThrowable shouldReturn 1
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
    "mapOrElse" - {
      "when Some" in {
        Option(1).mapOrElse(_ + 1, unusedThrowable) shouldReturn 2
      }
      "when None" in {
        None.mapOrElse((_: Int) => ???, 2) shouldReturn 2
      }
    }
  }
}
