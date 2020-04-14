package common.test

import org.scalatest.{Assertion, AsyncTestSuite, Succeeded}
import org.scalatest.TryValues._
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.Future

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.functor.ToFunctorOps
import scalaz.OptionT

import common.rich.RichFuture._

trait AsyncAuxSpecs extends AuxSpecs {self: AsyncTestSuite =>
  implicit class richFutureSpecs($: Future[_]) {
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
  implicit class richOptionTFutureSpecs[A]($: OptionT[Future, A]) {
    import org.scalatest.OptionValues._

    def mapValue(f: A => Assertion): Future[Assertion] = $.run.map(f apply _.value)
    def shouldEventuallyReturnNone(): Future[Assertion] = $.run.map(_ shouldReturn None)
  }
  def checkAll(f1: => Future[Assertion], f2: => Future[Assertion]): Future[Assertion] =
    f1 >> f2
  def checkAll(
      f1: => Future[Assertion], f2: => Future[Assertion], f3: => Future[Assertion]): Future[Assertion] =
    f1 >> f2 >> f3
  def checkAll(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
      f3: => Future[Assertion],
      f4: => Future[Assertion],
  ): Future[Assertion] = f1 >> f2 >> f3 >> f4
}
