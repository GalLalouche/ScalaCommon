package common.test

import org.scalatest.{Assertion, AsyncTestSuite, Succeeded}
import org.scalatest.TryValues._
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.Future

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.functor.ToFunctorOps

import common.rich.RichFuture._

trait AsyncAuxSpecs extends AuxSpecs {self: AsyncTestSuite =>
  implicit class richFutureSpecs[A]($: Future[_]) {
    def shouldNotFail(): Future[Assertion] = $ >| Succeeded
    def shouldFail(): Future[Assertion] = {
      val stackTrace = Thread.currentThread.getStackTrace.drop(2)
      $.toTry.map {t =>
        if (t.isFailure)
          Succeeded
        else {
          val e = new TestFailedException(s"Expected a failure but got <${t.success}>", 2)
          e.setStackTrace(stackTrace)
          throw e
        }
      }
    }
    def checkFailure(f: Throwable => Assertion): Future[Assertion] = $.toTry.map(_.failure.exception).map(f)
  }
}
