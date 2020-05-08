package common.rich.func

import org.scalatest.FreeSpec

import scalaz.{IdT, ListT, OptionT}
import scalaz.Id.Id
import scalaz.std.option.optionInstance
import common.rich.func.ToMoreMonadTransOps._

import common.test.AuxSpecs

class ToMoreMonadTransOpsTest extends FreeSpec with AuxSpecs {
  "List" in {
    List(1, 2, 3).hoistId shouldReturn ListT(Some(List(1, 2, 3)))
  }
  "Option" in {
    Option(1).hoistId shouldReturn OptionT(Some(Option(1)))
  }
  "Id" in {
    (1: Id[Int]).hoistId shouldReturn IdT(Some(1))
  }
}
