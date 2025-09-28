package common.rich.func

import org.scalatest.FreeSpec

import common.rich.func.ToTransableOps._
import scalaz.{\/, \/-, EitherT, ListT}
import scalaz.std.option.optionInstance

import common.test.AuxSpecs

class VersionDependentTransableInstancesTest extends FreeSpec with AuxSpecs {
  "List" in {
    List(1, 2, 3).hoistId shouldReturn ListT(Some(List(1, 2, 3)))
  }
  """\/""" - {
    """-\/""" in {
      val x: Transable.SwappedEitherT[Int, Option, String] = EitherT(Option(\/.left(1)))
      x.subFlatMap(e => \/-(e + 1)) shouldReturn EitherT(Option(\/.left(1)))
    }
    """\/-""" in {
      val x: Transable.SwappedEitherT[String, Option, Int] = EitherT(Option(\/.right(1)))
      x.subFlatMap(e => \/-(e + 1)) shouldReturn EitherT(Option(\/.right(2)))
    }
  }
}
