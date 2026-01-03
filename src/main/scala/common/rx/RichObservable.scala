package common.rx

import java.util.concurrent.atomic.AtomicInteger

import cats.data.OptionT
import rx.lang.scala.{Observable, Observer, Subscriber}
import rx.lang.scala.subjects.BehaviorSubject

import scala.concurrent.{ExecutionContext, Future, Promise}

import common.rich.RichT._
import common.rich.primitives.RichBoolean.richBoolean

object RichObservable {
  trait Unsubscribable[A] {
    def apply(a: A): Unit
  }
  implicit class RichSubscriber(private val $ : Subscriber[_]) extends AnyVal {
    def isSubscribed: Boolean = $.isUnsubscribed.isFalse
  }
  implicit class richObservable[A](private val $ : Observable[A]) extends AnyVal {
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
      $.flatMap(a =>
        Observable
          .from(f(a))
          .map(_ => a),
      )

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
    def groupByBuffer[B](f: A => B): Observable[(B, Seq[A])] = Observable { s =>
      val lastSubject = BehaviorSubject[(B, Seq[A])]()
      def publish(e: (B, Seq[A])): Unit = s.onNext(e._1 -> e._2.toVector.reverse)
      $.map(e => f(e) -> e)
        .scan[Option[(B, List[A])]](None) { case (agg, x) =>
          Some((agg match {
            case Some(agg @ (key, values)) =>
              if (key == x._1)
                key -> (x._2 :: values)
              else {
                publish(agg)
                x._1 -> List(x._2)
              }
            case None =>
              x._1 -> List(x._2)
          }) <| lastSubject.onNext)
        }
        .takeUntil(s.isSubscribed.isFalse.const)
        .subscribe(new Subscriber[Option[(B, List[A])]]() {
          override def onCompleted(): Unit = {
            lastSubject.subscribe(publish _)
            lastSubject.onCompleted()
            s.onCompleted()
          }
          override def onError(error: Throwable): Unit = s.onError(error)
        })
    }
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
