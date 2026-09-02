package common.rich

import java.util.concurrent.{ThreadFactory, TimeoutException}

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import common.rich.func.kats.ToMoreFunctorOps.toMoreFunctorOps

import common.rich.RichT._
import common.rich.primitives.RichBoolean.richBoolean

object RichFuture {
  implicit class richFutureBlocking[A](private val $ : Future[A]) extends AnyVal {
    def get: A = get(Duration.Inf)
    /** Throws [[TimeoutException]] if the future does not complete within the specified timeout. */
    def get(timeout: Duration): A = Await.result($, timeout)
    /** Returns [[None]] on timeout. */
    def getOpt(timeout: Duration): Option[A] =
      try Some(Await.result($, timeout))
      catch { case _: TimeoutException => None }
    def getFailure: Throwable = getFailure(Duration.Inf)
    /**
     * Throws (not returns!) [[TimeoutException]] if the future does not complete within the
     * specified timeout.
     */
    def getFailure(timeout: Duration): Throwable =
      Await.ready($, timeout).value.get match {
        case Success(v) =>
          throw new NoSuchElementException(s"Expected failure but was success <$v>")
        case Failure(e) => e
      }
  }
  implicit class richFuture[A]($ : Future[A])(implicit ec: ExecutionContext) {
    def |<(f: => Any): Future[A] = $ <| (_.onComplete(f.const))
    def onSuccessful(f: => Any): Future[A] = $ <| (_.onComplete(t => if (t.isSuccess) f else ()))
    def onFailed(f: => Any): Future[A] = $ <| (_.onComplete(t => if (t.isFailure) f else ()))

    def consumeTry(c: Try[A] => Any): Future[A] = toTry.listen(c).flatMap {
      case Success(t) => Future.successful(t)
      case Failure(e) => Future.failed(e)
    }
    def toTry: Future[Try[A]] = RichFuture.fromCallback($.onComplete)
    /** Future will fail after duration. Spawns a new thread to avoid starvation. */
    def withTimeLimit(duration: Duration): Future[A] = withTimeLimit(duration, new Thread(_))
    /** Future will fail after duration. Uses the provided thread factory to spawn a new thread. */
    def withTimeLimit(duration: Duration, tf: ThreadFactory): Future[A] = {
      if (duration.isFinite.isFalse || $.isCompleted)
        return $
      val res = Promise[A]()
      @volatile var thread: Thread = null
      $.onComplete { e =>
        if (thread != null)
          thread.interrupt()
        res.tryComplete(e)
      }
      thread = tf.newThread(() =>
        try {
          Thread.sleep(duration.toMillis)
          res.tryFailure(new TimeoutException(s"Future timed out after $duration"))
        } catch {
          case _: InterruptedException => /* Do nothing */
        },
      )
      thread.start()
      res.future
    }
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

  implicit class RichTryFuture[A]($ : Future[Try[A]])(implicit ec: ExecutionContext) {
    def fromTry: Future[A] = $.flatMap {
      case Success(s) => Future.successful(s)
      case Failure(e) => Future.failed(e)
    }
  }
}
