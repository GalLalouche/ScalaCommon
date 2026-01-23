package common.rich.primitives

import java.io.IOException

import org.scalatest.freespec.AnyFreeSpec

import common.rich.primitives.RichEither._
import common.test.AuxSpecs

class RichEitherTest extends AnyFreeSpec with AuxSpecs {
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
      "Left with custom typeclass for class" in {
        import RichEitherTest._
        val either: Either[CustomErrorType, Int] = Left(CustomErrorType("custom error"))
        val e = the[NoSuchElementException] thrownBy either.getOrThrow
        e.getMessage shouldReturn "CustomErrorType(custom error)"
      }
      "Left with custom typeclass for object" in {
        import RichEitherTest._
        val either: Either[CustomErrorType, Int] = Left(CustomErrorType("custom error"))
        val e = the[NoSuchElementException] thrownBy either.getOrThrow
        e.getMessage shouldReturn "CustomErrorType(custom error)"
      }
      "Right" in {
        val either: Either[IOException, Int] = Right(42)
        either.getOrThrow shouldReturn 42
      }
    }
  }
}

private object RichEitherTest {
  private implicit val customError: ToError[CustomErrorType] = ToError.fromToString[CustomErrorType]
  private case class CustomErrorType(msg: String) extends AnyVal
}
