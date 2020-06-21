package common.rich

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import common.rich.func.BetterFutureInstances._
import common.rich.func.ToMoreFunctorOps._

import common.rich.RichT._

object RichFuture {
  implicit class richFuture[A]($: Future[A])(implicit ec: ExecutionContext) {
    def |<(f: => Any): Future[A] = $ <| (_.onComplete(f.const))
    def onSuccessful(f: => Any): Future[A] = $ <| (_.onComplete(t => if (t.isSuccess) f else ()))
    def onFailed(f: => Any): Future[A] = $ <| (_.onComplete(t => if (t.isFailure) f else ()))
    def get: A = Await.result($, Duration.Inf)
    def getFailure: Throwable = {
      Await.ready($, Duration.Inf)
      val triedT: Try[A] = $.value.get
      try
        triedT.failed.get
      catch {
        case _: UnsupportedOperationException =>
          throw new UnsupportedOperationException(s"Expected failure but was success <${triedT.get}>")
      }
    }

    def consumeTry(c: Try[A] => Any): Future[A] = toTry listen c flatMap {
      case Success(t) => Future.successful(t)
      case Failure(e) => Future.failed(e)
    }
    def toTry: Future[Try[A]] = RichFuture.fromCallback($.onComplete)
  }

  def fromCallback[A](f: (A => Any) => Any): Future[A] = {
    val $ = Promise[A]()
    f($.success)
    $.future
  }
  def fromTryCallback[A](f: (Try[A] => Any) => Any): Future[A] = {
    val $ = Promise[A]()
    f($.complete)
    $.future
  }
}
