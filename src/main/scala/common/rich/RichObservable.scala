package common.rich
import rx.lang.scala.Observable

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, Promise}

object RichObservable {
  implicit class richObservable[T]($: Observable[T]) {
    /**
     * Returns a Future of a collection of all elements emitted by this observable. Future will not
     * complete if this observable does not complete.
     */
    def toFuture[Col[_]](implicit cbf: CanBuildFrom[Nothing, T, Col[T]]): Future[Col[T]] =
      richObservable($.to[Col]).firstFuture

    /**
     * Returns a Future of the first element emitted by this observable, or a failed Future that
     * fails with a NoSuchElementException if the observable is empty.
     */
    def firstFuture: Future[T] = {
      val p = Promise[T]()
      $.first.subscribe(p.success(_), p.failure(_))
      p.future
    }
  }
}
