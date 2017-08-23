package common.rich.func

import java.io.ByteArrayOutputStream

import common.AuxSpecs
import common.rich.func.MoreFoldable._
import org.scalatest.FreeSpec

class ToMoreFoldableOpsTest extends FreeSpec with AuxSpecs with ToMoreFoldableOps {
  "doForEach is left oriented" in {
    var s = ""
    Seq(1, 2, 3).doForEach(s += _)
    s shouldReturn "123"
  }
  "printPerLine should not overflow" in {
    Console.withOut(new ByteArrayOutputStream()){Seq.fill(100000)(1).printPerLine()}
  }
}
