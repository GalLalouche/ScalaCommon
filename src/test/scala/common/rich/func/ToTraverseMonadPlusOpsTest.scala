package common.rich.func

import common.AuxSpecs
import common.rich.RichFuture._
import org.scalatest.FreeSpec

import scala.concurrent.Future
import scalaz.std.ListInstances

class ToTraverseMonadPlusOpsTest extends FreeSpec with AuxSpecs with ToTraverseMonadPlusOps with MoreFutureInstances
    with ListInstances{
  import scala.concurrent.ExecutionContext.Implicits.global
  "filterTraverse" in {
    List(1, 2, 3).filterTraverse(x => Future successful x % 2 == 0).get shouldReturn List(2)
  }
}
