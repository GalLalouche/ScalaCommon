package common.rich.func

import org.scalatest.FreeSpec

import common.rich.func.ToTransableOps.toHoistIdOps
import scalaz.{IList, ListT}
import scalaz.std.option.optionInstance

import common.test.AuxSpecs

class ListTInstancesTest extends FreeSpec with AuxSpecs {
  "List" in {
    IList(1, 2, 3).hoistId shouldReturn ListT(Some(IList(1, 2, 3)))
  }
}
