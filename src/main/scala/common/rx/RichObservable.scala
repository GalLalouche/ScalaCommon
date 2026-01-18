package common.rx

import java.util.concurrent.atomic.AtomicInteger

import cats.data.OptionT
import cats.syntax.functor.toFunctorOps
import rx.lang.scala.{Observable, Observer, Subscriber}
import rx.lang.scala.schedulers.ImmediateScheduler

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

import common.rich.func.kats.ObservableInstances.observableInstances

import common.rich.primitives.RichBoolean.richBoolean

object RichObservable {
  trait Unsubscribable[A] {
    def apply(a: A): Unit
  }
  implicit class RichSubscriber(private val $ : Subscriber[_]) extends AnyVal {
    def isSubscribed: Boolean = $.isUnsubscribed.isFalse
  }
  implicit class richObservable[A](private val $ : Observable[A]) extends AnyVal {
    /** Blocks the current thread until the observable is finished. */
    def toVectorBlocking: Vector[A] = buildBlocking(Vector.newBuilder[A])
    /** Blocks the current thread until the observable is finished. */
    def buildBlocking[CC](builder: mutable.Builder[A, CC]): CC = {
      foreachBlocking(builder += _)
      builder.result()
    }
    /** Blocks the current thread until the observable is finished. */
    def foreachBlocking(f: A => Unit): Unit = {
      var error: Throwable = null
      $.subscribeOn(ImmediateScheduler())
        .observeOn(ImmediateScheduler())
        .subscribe(new Observer[A] {
          override def onNext(value: A): Unit = f(value)
          override def onError(e: Throwable): Unit = error = e
          override def onCompleted(): Unit = ()
        })
      if (error != null)
        throw error
    }
    /**
     * Returns a Future of the first element emitted by this observable, or a failed Future that
     * fails with a NoSuchElementException if the observable is empty.
     */
    def firstFuture: Future[A] = {
      val p = Promise[A]()
      $.first.subscribe(p.success, p.failure)
      p.future
    }

    /**
     * Subscribes, but also returns a future that will complete when this Observable completes. If
     * the observable calls onError, the failure will propagate to the future.
     */
    def subscribeWithNotification(observer: Observer[A]): Future[Unit] = {
      val promise = Promise[Unit]()
      $.subscribe(new Observer[A] {
        override def onNext(value: A): Unit = observer.onNext(value)
        override def onError(error: Throwable): Unit = {
          observer.onError(error)
          promise.failure(error)
        }
        override def onCompleted(): Unit = {
          observer.onCompleted()
          promise.success(())
        }
      })
      promise.future
    }

    def flattenElements[B](implicit ev: A <:< Iterable[B]): Observable[B] = $.flatMapIterable(ev)

    /** Note that if the future action fails, the returned Observable will fail as well. */
    def doOnNextAsync(f: A => Future[_])(implicit ec: ExecutionContext): Observable[A] =
      $.flatMap(a => Observable.from(f(a)).map(_ => a))

    def filterFuture(p: A => Future[Boolean])(implicit ec: ExecutionContext): Observable[A] =
      $.flatMap(a => Observable.from(p(a)).filter(identity).map(_ => a))

    def mapFutureOption[B](f: A => OptionT[Future, B])(implicit
        ec: ExecutionContext,
    ): Observable[B] =
      $.flatMap(a => RichObservable.from(f(a)))

    // TODO This should be possible for all Traverse?
    /**
     * Emits an observable which groups successive elements together according to some key function.
     * Unlike groupBy, only successive elements can be in the same group.
     */
    def groupByBuffer[B](f: A => B): Observable[(B, Seq[A])] =
      $.fproductLeft(f).lift[(B, Seq[A])](out =>
        new Subscriber[(B, A)] {
          private val buffer = Vector.newBuilder[A]
          private def flush(key: Option[B]): Unit = key.foreach { value =>
            out.onNext(value, buffer.result().ensuring(_.nonEmpty))
            buffer.clear()
          }
          private var lastKey: Option[B] = None
          override def onNext(elem: (B, A)): Unit = {
            if (out.isUnsubscribed) {
              unsubscribe()
              return
            }
            flush(lastKey.filter(_ != elem._1))
            lastKey = Some(elem._1)
            buffer += elem._2
          }

          override def onError(ex: Throwable): Unit = out.onError(ex)

          override def onCompleted(): Unit = {
            flush(lastKey)
            out.onCompleted()
          }
        },
      )
  }

  def register[A](callback: (A => Unit) => Unit, unsubscribe: () => Any = null): Observable[A] =
    Observable[A] { s =>
      callback(s.onNext)
      if (unsubscribe != null)
        s.add { unsubscribe(); () }
    }

  def registerUnsubscribable[A, S: Unsubscribable](callback: (A => Unit) => S): Observable[A] =
    Observable[A] { subscriber =>
      val subscription = callback(subscriber.onNext)
      subscriber.add(implicitly[Unsubscribable[S]].apply(subscription))
    }

  // os.reduce(_ merge _) isn't stack safe.
  def concat[A](os: TraversableOnce[Observable[A]]): Observable[A] = {
    val seq = os.toVector
    val size = seq.size
    if (seq.isEmpty) Observable.empty
    else
      Observable[A] { s =>
        val completed: AtomicInteger = new AtomicInteger(0)
        val subs = seq.map(_.subscribe(new Observer[A] {
          override def onNext(value: A) = s.onNext(value)
          override def onError(error: Throwable) = s.onError(error)
          override def onCompleted() = if (completed.incrementAndGet() >= size) s.onCompleted()
        }))
        s.add(subs.foreach(_.unsubscribe()))
      }
  }

  def from[A](fo: OptionT[Future, A])(implicit ec: ExecutionContext): Observable[A] = Observable
    .from(fo.value)
    .flatMap {
      case Some(value) => Observable.just(value)
      case None => Observable.empty
    }
  def continually[A](initial: => A): Observable[A] = Observable.apply { s =>
    val value = initial
    while (s.isSubscribed)
      s.onNext(value)
    s.onCompleted()
  }
  def iterate[A](initial: => A)(f: A => A): Observable[A] =
    continually(initial).scan[A](initial)((e, _) => f(e))
}
