package common.rich.func.scalazz

import org.scalatest.FreeSpec

import common.rich.func.scalazz.ToTransableOps.toHoistIdOps
import scalaz.{IdT, OptionT}
import scalaz.Id.Id
import scalaz.std.option.optionInstance

import common.test.AuxSpecs

class ToTransableOpsTest extends FreeSpec with AuxSpecs {
  "Option" in {
    Option(1).hoistId shouldReturn OptionT(Some(Option(1)))
  }
  "Id" in {
    (1: Id[Int]).hoistId shouldReturn IdT(Some(1))
  }
}
