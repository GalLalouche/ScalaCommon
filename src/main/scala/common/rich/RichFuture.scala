package common.rich

import common.rich.func.ToMoreFunctorOps._
import scalaz.std.scalaFuture.futureInstance

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object RichFuture {
  implicit class richFuture[T]($: Future[T])(implicit ec: ExecutionContext) {
    def get: T = Await.result($, Duration.Inf)
    def getFailure: Throwable = {
      Await.ready($, Duration.Inf)
      val triedT: Try[T] = $.value.get
      try {
        triedT.failed.get
      } catch {
        case _: UnsupportedOperationException =>
          throw new UnsupportedOperationException(s"Expected failure but was success <${triedT.get}>")
      }
    }

    def consumeTry(c: Try[T] => Any): Future[T] = toTry listen c flatMap {
      case Success(t) => Future.successful(t)
      case Failure(e) => Future.failed(e)
    }
    def toTry: Future[Try[T]] = {
      val p = Promise[Try[T]]()
      $ onComplete p.success
      p.future
    }
  }
}
