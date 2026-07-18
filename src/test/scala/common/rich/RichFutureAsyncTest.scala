package common.rich

import java.util.concurrent.TimeoutException

import org.scalatest.OneInstancePerTest
import org.scalatest.compatible.Assertion
import org.scalatest.concurrent.TimeLimits
import org.scalatest.freespec.AsyncFreeSpec

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.DurationInt
import scala.util.Success

import common.rich.func.scalazz.BetterFutureInstances._
import scalaz.syntax.functor.ToFunctorOps

import common.rich.RichFuture._
import common.test.AuxSpecs

class RichFutureAsyncTest
    extends AsyncFreeSpec
    with AuxSpecs
    with OneInstancePerTest
    with TimeLimits {
  private def success: Future[Int] = Future.successful(1)
  private val exception = new Exception("Derp")
  private def failure: Future[Int] = Future.failed(exception)
  private def invoked(f: (=> Any) => Future[_]) = {
    var x = 0
    f(x += 1).toTry.>|(x shouldReturn 1)
  }
  private def notInvoked(f: (=> Any) => Future[_]) = {
    var x = 0
    f(x += 1).toTry.>|(x shouldReturn 0)
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
        success
          .consumeTry(e => succeeded = e.isSuccess)
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
    "timedGet" - {
      "Already completed future" in {
        success.withTimeLimit(scala.concurrent.duration.Duration.Inf).map(_.shouldReturn(1))
      }
      def aux(f: => Future[Int], shouldFinish: Boolean): Future[Assertion] = failAfter(1.second) {
        @volatile var thread: Thread = null
        f.withTimeLimit(
          200.millis,
          r => {
            thread = new Thread(r)
            thread
          },
        ).toTry
          .map { res =>
            Thread.sleep(100) // gives enough time for the thread be interrupted
            thread.isAlive.shouldReturn(false)
            if (shouldFinish) res.shouldReturn(Success(1))
            else res.failed.get shouldBe a[TimeoutException]
          }
      }
      def delayedFuture(delay: Long): Future[Int] = {
        val p = Promise[Int]()
        new Thread(() => {
          Thread.sleep(delay)
          p.success(1)
        }).start()
        p.future
      }
      "Eventually completing future" in {
        aux(delayedFuture(10), shouldFinish = true)
      }
      "Slow future" in {
        aux(delayedFuture(5000), shouldFinish = false)
      }
      "Never future" in {
        aux(Future.never, shouldFinish = false)
      }
    }
  }
}
