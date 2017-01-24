package common.rich

import common.rich.RichT._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

object RichFuture {
  implicit class richFuture[T]($: Future[T])(implicit ec: ExecutionContext) {
    // gives better error message when the filter fails
    def filterWith(p: T => Boolean, message: String): Future[T] = filterWithMessage(p, e => message)
    def filterWithStacktrace(p: T => Boolean, message: String = "Failed filter") = {
      val ex = new NoSuchElementException(message)
      ex.setStackTrace(ex.getStackTrace drop 1)
      $.flatMap(e => if (p(e)) $ else Future failed ex)
    }
    // implicits suck with overloads it seems
    def filterWithMessage(p: T => Boolean, message: T => String): Future[T] = $.flatMap(e =>
      if (p(e))
        $
      else
        Future failed new NoSuchElementException(message(e)))
    def get: T = Await.result($, Duration.Inf)
    def getFailure: Throwable = {
      Await.ready($, Duration.Inf)
      val triedT: Try[T] = $.value.get
      try {
        triedT.failed.get
      } catch {
        case _: UnsupportedOperationException => throw new UnsupportedOperationException(s"Expected failure but was success <${triedT.get}>")
      }
    }
    def consume(c: T => Any): Future[T] = $.map(e => {c(e); e})
    // like recover, but doesn't care about the failure
    def orElse(t: => T): Future[T] = $.recover { case _ => t }
    // implicits suck with overloads it seems
    def orElseTry(t: => Future[T]): Future[T] = $.recoverWith { case _ => t }
  }
  implicit class richOptionFuture[T]($: Future[Option[T]])(implicit ec: ExecutionContext) {
    def ifNone(t: => T): Future[T] = $.map(_ getOrElse t)
    def ifNoneTry(t: => Future[T]): Future[T] = $.flatMap(_ map Future.successful getOrElse t)
    def filterFuture(p: T => Future[Boolean]): Future[Option[T]] = $.flatMap {
      case None => Future successful None
      case o@Some(e) => p(e).map(_.const |> o.filter)
    }
  }
}
