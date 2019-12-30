package common.rich

import java.util.concurrent.Executors

import org.scalatest.FreeSpec

import scala.concurrent.{ExecutionContext, Future}

import common.rich.RichFuture._
import common.test.AuxSpecs

class RichFutureTest extends FreeSpec with AuxSpecs {
  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
  private def success: Future[Int] = Future {
    Thread sleep 10
    5
  }
  private val exception = new Exception("Derp")
  private def failure: Future[Int] = Future {
    Thread sleep 10
    throw exception
  }

  "get" - {
    "success" in {
      success.get shouldReturn 5
    }
    "failure" in {
      an[Exception] should be thrownBy failure.get
    }
  }

  "getFailure" - {
    "when success should throw" in {
      an[UnsupportedOperationException] should be thrownBy success.getFailure
    }
    "when failure should return the error" in {
      val f = failure
      val e = f.getFailure
      e shouldReturn exception
    }
  }
}
