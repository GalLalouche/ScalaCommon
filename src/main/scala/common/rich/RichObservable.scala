package common.rich

import java.util.concurrent.atomic.AtomicInteger

import rx.lang.scala.{Observable, Observer}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.higherKinds

object RichObservable {
  trait Unsubscribable[A] {
    def apply(a: A): Unit
  }
  implicit class richObservable[A](private val $: Observable[A]) extends AnyVal {
    /**
     * Returns a Future of a collection of all elements emitted by this observable. Future will not
     * complete if this observable does not complete.
     */
    def toFuture[Coll[_]](implicit cbf: CanBuildFrom[Nothing, A, Coll[A]]): Future[Coll[A]] =
      richObservable($.to[Coll]).firstFuture

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
     * Subscribes, but also returns a future that will complete when this Observable completes.
     * If the observable calls onError, the failure will propagate to the future.
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
    def doOnNextAsync(f: A => Future[_])(implicit ec: ExecutionContext): Observable[A] = $.flatMap(a =>
      Observable
          .from(f(a))
          .map(_ => a)
    )
  }

  def register[A](callback: (A => Unit) => Unit, unsubscribe: () => Any = null): Observable[A] =
    Observable[A] {s =>
      callback(s.onNext)
      if (unsubscribe != null)
        s.add {unsubscribe(); ()}
    }

  def registerUnsubscribable[A, S: Unsubscribable](callback: (A => Unit) => S): Observable[A] =
    Observable[A] {subscriber =>
      val subscription = callback(subscriber.onNext)
      subscriber.add(implicitly[Unsubscribable[S]].apply(subscription))
    }

  // os.reduce(_ merge _) isn't stack safe.
  def concat[A](os: TraversableOnce[Observable[A]]): Observable[A] = {
    val seq = os.toVector
    val size = seq.size
    if (seq.isEmpty) Observable.empty else Observable[A] {s =>
      val completed: AtomicInteger = new AtomicInteger(0)
      val subs = seq.map(_.subscribe(new Observer[A] {
        override def onNext(value: A) = s.onNext(value)
        override def onError(error: Throwable) = s.onError(error)
        override def onCompleted() = if (completed.incrementAndGet() >= size) s.onCompleted()
      }))
      s.add(subs.foreach(_.unsubscribe()))
    }
  }
}
