package common.rich.func.kats

import cats.Id
import cats.data.{EitherT, IdT, OptionT}
import cats.instances.option.catsStdInstancesForOption
import org.scalatest.FreeSpec

import common.rich.func.kats.ToTransableOps.toHoistIdOps

import common.test.AuxSpecs

class ToTransableOpsTest extends FreeSpec with AuxSpecs {
  "Option" in {
    Option(1).hoistId shouldReturn OptionT(Some(Option(1)))
  }
  "Id" in {
    (1: Id[Int]).hoistId shouldReturn IdT(Some(1))
  }
  // Explicit type annotations needed for 2.12.
  "Either" - {
    "Right" in {
      (Right(1): Either[String, Int])
        .hoistId[Option, EitherT[*[_], String, *]] shouldReturn EitherT[Option, String, Int](
        Some(Right(1)),
      )
    }
    "Left" in {
      (Left(1): Either[Int, String])
        .hoistId[Option, EitherT[*[_], Int, *]] shouldReturn EitherT[Option, Int, String](
        Some(Left(1)),
      )
    }
  }
}
