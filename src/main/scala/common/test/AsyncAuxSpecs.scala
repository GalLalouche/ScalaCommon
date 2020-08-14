package common.test

import org.scalatest.{Assertion, AsyncTestSuite, Succeeded}
import org.scalatest.TryValues._
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.Future

import scalaz.{OptionT, StreamT}
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.functor.ToFunctorOps
import common.rich.func.BetterFutureInstances._

import common.rich.RichFuture._

// TODO FunctorAuxSpecs?
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
    def failureShouldEventuallyReturn(e: => Throwable): Future[Assertion] =
      $.toTry.map(_.failure.exception).shouldEventuallyReturn(e)
  }
  implicit class richTypedFutureSpecs[A]($: Future[A]) {
    def shouldEventuallyReturn(t: => A): Future[Assertion] = $.map(_ shouldReturn t)
  }
  implicit class richOptionTFutureSpecs[A]($: OptionT[Future, A]) {
    import org.scalatest.OptionValues._

    def mapValue(f: A => Assertion): Future[Assertion] = $.run.map(f apply _.value)
    def shouldEventuallyReturnNone(): Future[Assertion] = $.run shouldEventuallyReturn None
    def valueShouldEventuallyReturn(a: => A): Future[Assertion] = $.run.map(_.value shouldReturn a)
  }
  implicit class richStreamTFutureSpecs[A]($: StreamT[Future, A]) {
    def mapValue(f: Stream[A] => Assertion): Future[Assertion] = $.toStream.map(f.apply)
    def valueShouldEventuallyReturn(s: Stream[A]): Future[Assertion] = mapValue(_ shouldReturn s)
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
