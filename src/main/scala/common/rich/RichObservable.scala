package common.rich

import java.util.concurrent.atomic.AtomicInteger

import rx.lang.scala.{Observable, Observer}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, Promise}
import scala.language.higherKinds

object RichObservable {
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

    def flattenElements[B](implicit ev: A <:< Iterable[B]): Observable[B] =
      $.flatMap(e => Observable.from(ev(e)))
  }

  def register[A](callback: (A => Unit) => Unit): Observable[A] = Observable[A](s => callback(s.onNext))

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
