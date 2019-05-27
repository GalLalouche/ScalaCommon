package common.rich

import common.AuxSpecs
import common.rich.RichFuture._
import org.scalatest.FreeSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class RichFutureTest extends FreeSpec with AuxSpecs {
  val success: Future[Int] = Future successful 1
  val exception = new Exception("Derp")
  val failure: Future[Int] = Future failed exception
  def shouldFail(f: Future[Any]) = {
    Await.ready(f, Duration.Inf)
    val t = f.value.get
    t.isFailure shouldReturn true
  }
  "RichFuture" - {
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
