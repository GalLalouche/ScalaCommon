package common.rx

import rx.lang.scala.Observable

import scala.collection.generic.CanBuildFrom
import scala.concurrent.Future

object RichObservableSpecVer {
  implicit class richObservableSpecVer[A](private val $ : Observable[A]) extends AnyVal {
    /**
     * Returns a Future of a collection of all elements emitted by this observable. Future will not
     * complete if this observable does not complete.
     */
    def toFuture[Coll[_]](implicit cbf: CanBuildFrom[Nothing, A, Coll[A]]): Future[Coll[A]] =
      RichObservable.richObservable($.to[Coll]).firstFuture
  }
}
