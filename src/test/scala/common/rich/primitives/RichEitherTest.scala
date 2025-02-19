package common.rich.primitives

import java.io.IOException

import org.scalatest.FreeSpec

import common.rich.primitives.RichEither._
import common.test.AuxSpecs

class RichEitherTest extends FreeSpec with AuxSpecs {
  "getOrThrow" - {
    "String" - {
      "Left" in {
        val e =
          the[NoSuchElementException] thrownBy (Left("foobar"): Either[String, Int]).getOrThrow
        e.getMessage shouldReturn "foobar"
      }
      "Right" in {
        (Right(42): Either[String, Int]).getOrThrow shouldReturn 42
      }
    }
    "Throwable" - {
      "Left" in {
        val t = new IOException("foobar")
        val either: Either[IOException, Int] = Left(t)
        val e = the[IOException] thrownBy either.getOrThrow
        e shouldReturn t
      }
      "Right" in {
        val either: Either[IOException, Int] = Right(42)
        either.getOrThrow shouldReturn 42
      }
    }
  }
}
