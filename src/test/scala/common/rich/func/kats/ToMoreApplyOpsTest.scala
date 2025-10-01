package common.rich.func.kats

import org.scalatest.FreeSpec

import common.rich.func.kats.ToMoreApplyOps.toMoreApplyOps

import common.test.AuxSpecs

class ToMoreApplyOpsTest extends FreeSpec with AuxSpecs {
  private val noneInt: Option[Int] = None
  private val someInt: Option[Int] = Some(42)
  private val noneString: Option[String] = None
  private val someString: Option[String] = Some("foobar")
  "<<*" - {
    "none" in { noneInt <<* ??? shouldReturn None }
    "some <<* none" in { someInt <<* noneString shouldReturn None }
    "some <<* some" in { someInt <<* someString shouldReturn someInt }
  }
  ">>*" - {
    "none" in { noneInt *>> ??? shouldReturn None }
    "some >>* none" in { someInt *>> noneString shouldReturn None }
    "some >>* some" in { someInt *>> someString shouldReturn someString }
  }
}
