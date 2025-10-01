package common.rich

import org.scalatest.{AsyncFreeSpec, OneInstancePerTest}

import scala.concurrent.Future

import scalaz.syntax.functor.ToFunctorOps
import common.rich.func.scalazz.BetterFutureInstances._

import common.rich.RichFuture._
import common.test.AuxSpecs

class RichFutureAsyncTest extends AsyncFreeSpec with AuxSpecs with OneInstancePerTest {
  private def success: Future[Int] = Future successful 1
  private val exception = new Exception("Derp")
  private def failure: Future[Int] = Future failed exception
  private def invoked(f: (=> Any) => Future[_]) = {
    var x = 0
    f {x += 1}.toTry.>|(x shouldReturn 1)
  }
  private def notInvoked(f: (=> Any) => Future[_]) = {
    var x = 0
    f {x += 1}.toTry.>|(x shouldReturn 0)
  }
  "RichFuture" - {
    "|<" - {
      "success" in invoked(success.|<)
      "failure" in invoked(failure.|<)
    }
    "onSuccessful" - {
      "success" in invoked(success.onSuccessful)
      "failure" in notInvoked(failure.onSuccessful)
    }
    "onFailed" - {
      "success" in notInvoked(success.onFailed)
      "failure" in invoked(failure.onFailed)
    }
    "consumeTry" - {
      "success" in {
        var succeeded = false
        success.consumeTry(e => succeeded = e.isSuccess)
            .map(_.shouldReturn(1) && succeeded.shouldReturn(true))
      }
      "failure" in {
        var succeeded = true
        failure
            .consumeTry(e => succeeded = e.isSuccess)
            .toTry
            .map(_ => succeeded.shouldReturn(false))
      }
    }
  }
}
