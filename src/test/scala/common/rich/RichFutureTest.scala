package common.rich

import org.scalatest.{FreeSpec, OneInstancePerTest}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

import common.AuxSpecs
import common.rich.RichFuture._

class RichFutureTest extends FreeSpec with AuxSpecs with OneInstancePerTest {
  private val success: Future[Int] = Future successful 1
  private val exception = new Exception("Derp")
  private val failure: Future[Int] = Future failed exception
  def shouldFail(f: Future[Any]) = {
    Await.ready(f, Duration.Inf)
    val t = f.value.get
    t.isFailure shouldReturn true
  }
  def invoked(f: (=> Any) => Future[_]): Unit = {
    var x = 0
    f {x += 1}.toTry.get
    x shouldReturn 1
  }
  def notInvoked(f: (=> Any) => Future[_]): Unit = {
    var x = 0
    f {x += 1}.toTry.get
    x shouldReturn 0
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
    "get" - {
      "success" in {
        val f = Future {
          Thread.sleep(10)
          5
        }
        f.get shouldReturn 5
      }
      "failure" in {
        val f = Future {
          Thread.sleep(10)
          throw new Exception()
        }
        an[Exception] should be thrownBy f.get
      }
    }
    "getFailure" - {
      "when success should throw" in {
        val f = Future[Int](1)
        an[UnsupportedOperationException] should be thrownBy f.getFailure
      }
      "when failure should return the error" in {
        val f = failure
        val e = f.getFailure
        e shouldReturn exception
      }
    }
    "consumeTry" - {
      "success" in {
        var succeeded = false
        val value = success.consumeTry(e => succeeded = e.isSuccess).get
        value shouldReturn 1
        succeeded shouldReturn true
      }
      "failure" in {
        var succeeded = true
        val value = failure.consumeTry(e => succeeded = e.isSuccess).getFailure
        value shouldReturn exception
        succeeded shouldReturn false
      }
    }
  }
}
