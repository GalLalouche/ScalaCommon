package common.rich

import java.util.concurrent.{Executors, TimeoutException}

import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

import common.rich.RichFuture._
import common.test.AuxSpecs

class RichFutureTest extends AnyFreeSpec with AuxSpecs {
  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
  private def success: Future[Int] = Future {
    Thread.sleep(10)
    5
  }
  private val exception = new Exception("Derp")
  private def failure: Future[Int] = Future {
    Thread.sleep(10)
    throw exception
  }

  "get" - {
    "success" in {
      success.get shouldReturn 5
    }
    "failure" in {
      an[Exception] should be thrownBy failure.get
    }
    "timedGet" - {
      "success" in {
        success.get(1.second) shouldReturn 5
      }
      "failure" in {
        an[Exception] should be thrownBy failure.get(1.second)
      }
      "timeout" in {
        a[TimeoutException] should be thrownBy success.get(1.millisecond)
      }
    }
    "getOpt" - {
      "success" in {
        success.getOpt(1.second) shouldReturn Some(5)
      }
      "timeout on failure" in {
        failure.getOpt(1.millisecond) shouldReturn None
      }
      "timeout on success" in {
        success.getOpt(1.millisecond) shouldReturn None
      }
      "failure throws" in {
        val e = new Exception("Derp2")
        val actual = the[Exception] thrownBy Future.failed(e).getOpt(1.second)
        actual shouldBe theSameInstanceAs(e)
      }
    }
  }

  "getFailure" - {
    "when success should throw" in {
      a[NoSuchElementException] should be thrownBy success.getFailure
    }
    "when failure should return the error" in {
      val f = failure
      val e = f.getFailure
      e shouldReturn exception
    }
    "When timing out should throw" in {
      a[TimeoutException] should be thrownBy Future.never.getFailure(1.millisecond)
    }
  }
}
