package common.rich.func.scalazz

import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.scalazz.ToTransableOps.toHoistIdOps
import scalaz.{-\/, \/-, EitherT, IList, ListT}
import scalaz.std.option.optionInstance

import common.test.AuxSpecs

class VersionDependentTransableInstancesTest extends AnyFreeSpec with AuxSpecs {
  "List" in {
    IList(1, 2, 3).hoistId shouldReturn ListT(Some(IList(1, 2, 3)))
  }
  """\/""" - {
    """-\/""" in { -\/(1).hoistId shouldReturn EitherT[Int, Option, Nothing](Some(-\/(1))) }
    """\/-""" in { \/-(1).hoistId shouldReturn EitherT[Nothing, Option, Int](Some(\/-(1))) }
  }
}
