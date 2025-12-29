package common.test

import cats.data.OptionT
import cats.implicits.{catsSyntaxApplyOps, catsSyntaxFlatMapOps, toFunctorOps}
import org.scalatest.{Assertion, AsyncTestSuite, Succeeded}
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.TryValues._
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.Future

import common.rich.RichFuture._

// TODO FunctorAuxSpecs?
trait AsyncAuxSpecs extends AuxSpecs { self: AsyncTestSuite =>
  implicit class richFutureSpecs($ : Future[_]) {
    def shouldNotFail(): Future[Assertion] = $.as(Succeeded)
    def shouldFail(): Future[Assertion] = {
      val stackTrace = Thread.currentThread.getStackTrace.drop(2)
      $.toTry.map { t =>
        if (t.isFailure)
          Succeeded
        else {
          val e = new TestFailedException(s"Expected a failure but got <${t.success}>", 2)
          e.setStackTrace(stackTrace)
          throw e
        }
      }
    }
    def checkFailure(f: Throwable => Assertion): Future[Assertion] =
      $.toTry.map(_.failure.exception).map(f)
    def failureShouldEventuallyReturn(e: => Throwable): Future[Assertion] =
      $.toTry.map(_.failure.exception).shouldEventuallyReturn(e)
  }
  implicit class richTypedFutureSpecs[A]($ : Future[A]) {
    def shouldEventuallyReturn(t: => A): Future[Assertion] = $.map(_ shouldReturn t)
  }
  implicit class richOptionTFutureSpecs[A]($ : OptionT[Future, A]) {

    def mapValue(f: A => Assertion): Future[Assertion] = $.value.map(f apply _.value)
    def shouldEventuallyReturnNone(): Future[Assertion] = $.value shouldEventuallyReturn None
    def valueShouldEventuallyReturn(a: => A): Future[Assertion] =
      $.value.map(_.value shouldReturn a)
  }
  // All these manual overloads are necessary since scala can't do varargs of by-name params.
  def checkAll(f1: => Future[Assertion], f2: => Future[Assertion]): Future[Assertion] = f1 >> f2
  def checkAll(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
      f3: => Future[Assertion],
  ): Future[Assertion] = f1 >> f2 >> f3
  def checkAll(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
      f3: => Future[Assertion],
      f4: => Future[Assertion],
  ): Future[Assertion] = f1 >> f2 >> f3 >> f4
  def checkAll(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
      f3: => Future[Assertion],
      f4: => Future[Assertion],
      f5: => Future[Assertion],
  ): Future[Assertion] = f1 >> f2 >> f3 >> f4 >> f5

  def checkAllParallel(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
  ): Future[Assertion] = f1 *> f2
  def checkAllParallel(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
      f3: => Future[Assertion],
  ): Future[Assertion] = f1 *> f2 *> f3
  def checkAllParallel(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
      f3: => Future[Assertion],
      f4: => Future[Assertion],
  ): Future[Assertion] = f1 *> f2 *> f3 *> f4
  def checkAllParallel(
      f1: => Future[Assertion],
      f2: => Future[Assertion],
      f3: => Future[Assertion],
      f4: => Future[Assertion],
      f5: => Future[Assertion],
  ): Future[Assertion] = f1 *> f2 *> f3 *> f4 *> f5
}
