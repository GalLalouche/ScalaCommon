package common.concurrency

import common.rich.RichT._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
import scalaz.MonadPlus

/**
* A MonadicPlus () => A. This isn't exactly a PartialFunction, since it's either defined or undefined for all (well,
* singular) inputs. This is preferable to Future since it doesn't require an ExplicitContext for it operations.
* Convenient methods for converting to and from scala.concurrent.Future are provided.
* There are two variants of Lazy: memoized and async. Memoized will delay all calulations until join() is invoked. Async
* will eagerly perform all calculations (including maps and flatMaps) but requires an ExplicitContext for construction
* (but not for any subsequent operations; it reuses the same ExplicitContext).
*/
trait Lazy[+A] {
  /** Blocks until the value is available. */
  def join(): A
  def toFuture(implicit ec: ExecutionContext): Future[A]
  def map[B](f: A => B): Lazy[B]
  def flatMap[B](f: A => Lazy[B]): Lazy[B]

  protected def filterWithException(p: A => Boolean, exceptionMaker: A => Throwable): Lazy[A]
  def filter(p: A => Boolean): Lazy[A] = filterWithException(p, new NoSuchElementException("Filtered").const)
  def withFilter(p: A => Boolean): Lazy[A] = filter(p)
  def filterWithStacktrace(p: A => Boolean, message: String = "Failed filter"): Lazy[A] = {
    val ex = new NoSuchElementException(message)
    ex.setStackTrace(ex.getStackTrace drop 1)
    filterWithException(p, ex.const)
  }
  def filterWithMessage(p: A => Boolean, message: String): Lazy[A] =
    filterWithException(p, new NoSuchElementException(message).const)
  def filterWithMessage(p: A => Boolean, message: A => String): Lazy[A] =
    filterWithException(p, message(_) |> (new NoSuchElementException(_)))
  def toTry(): Try[A]
  def toOption(): Option[A] = toTry().toOption
  def isSuccess(): Boolean = toTry().isSuccess
  def orElse[B >: A](other: => B): Lazy[B]
  def orElse[B >: A](other: Lazy[B]): Lazy[B] = orElse(other.join())
}

object Lazy {
  def failed(t: Throwable): Lazy[Nothing] = new FailedLazy(t)
  private class FailedLazy(t: Throwable) extends Lazy[Nothing] {
    override def join(): Nothing = throw t
    override def toFuture(implicit ec: ExecutionContext): Future[Nothing] = Future failed t
    override def map[B](f: Nothing => B): Lazy[B] = this
    override def flatMap[B](f: Nothing => Lazy[B]): Lazy[B] = this
    override protected def filterWithException(p: Nothing => Boolean, exceptionMaker: Nothing => Throwable) = this
    override def toTry(): Try[Nothing] = Failure(t)
    override def orElse[B >: Nothing](other: => B): Lazy[B] = Lazy(other)
  }
  def apply[A](a: => A): Lazy[A] = new MemoizedLazy(a)
  private class MemoizedLazy[A](a: => A) extends Lazy[A] {
    private lazy val asTry = Try(a)
    override def join() = asTry.get
    override def toFuture(implicit ec: ExecutionContext): Future[A] = Future(join())
    override def map[B](f: A => B): Lazy[B] = Lazy(f(join()))
    override def flatMap[B](f: A => Lazy[B]): Lazy[B] = Lazy(f(join()).join())

    override protected def filterWithException(p: A => Boolean, exceptionMaker: A => Throwable): Lazy[A] = Lazy(asTry match {
      case Failure(exception) => throw exception
      case Success(value) => if (p(value)) value else throw exceptionMaker(value)
    })
    override def toTry(): Try[A] = asTry
    def orElse[B >: A](other: => B): Lazy[B] = Lazy(toOption() getOrElse other)
  }
  /** Starts calculating the value immediately on the supplied context. */
  def async[A](a: => A)(implicit ec: ExecutionContext): Lazy[A] = {
    val promise = Promise[A]
    lazy val lazyTry = Try(a)
    ec.execute(() => promise complete lazyTry)
    fromFuture(promise.future)
  }
  private class LazyAsync[A](future: Future[A])(implicit ec: ExecutionContext) extends Lazy[A] {
    import common.rich.RichFuture._
    override def join() = future.get
    override def toFuture(implicit unused: ExecutionContext): Future[A] = future
    private def fromFutureAsync[B](f: Future[B]): Lazy[B] = {
      val mappedPromise = Promise[B]()
      f.onComplete(mappedPromise.complete)
      fromFuture(mappedPromise.future)
    }
    override def map[B](f: A => B) = this.future.map(f) |> fromFutureAsync
    override def flatMap[B](f: A => Lazy[B]): Lazy[B] = this.future.flatMap(f(_).toFuture) |> fromFutureAsync
    override protected def filterWithException(p: A => Boolean, exceptionMaker: A => Throwable) =
      future.flatMap(e => if (p(e)) Future successful e else Future failed exceptionMaker(e)) |> fromFutureAsync
    override def orElse[B >: A](other: => B): Lazy[B] = future.recover {case _ => other} |> fromFutureAsync
    override def toTry(): Try[A] = Try(join())
  }

  def fromFuture[A](f: Future[A])(implicit ec: ExecutionContext): Lazy[A] = new LazyAsync(f)

  implicit object LazyMonadPlus extends MonadPlus[Lazy] {
    override def bind[A, B](fa: Lazy[A])(f: A => Lazy[B]): Lazy[B] = fa flatMap f
    override def point[A](a: => A): Lazy[A] = Lazy(a)
    override def empty[A]: Lazy[A] = failed(new NoSuchElementException("empty"))
    override def plus[A](a: Lazy[A], b: => Lazy[A]): Lazy[A] = a orElse b
  }
}
