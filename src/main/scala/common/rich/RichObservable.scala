package common.rich

import rx.lang.scala.Observable

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
}
