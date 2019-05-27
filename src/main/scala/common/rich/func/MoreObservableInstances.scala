package common.rich.func
import rx.lang.scala.Observable

import scala.language.higherKinds
import scalaz.MonadPlus

trait MoreObservableInstances {
  implicit object ObservableMonadPlus extends MonadPlus[Observable] {
    override def bind[A, B](fa: Observable[A])(f: A => Observable[B]): Observable[B] = fa flatMap f
    override def plus[A](a: Observable[A], b: => Observable[A]): Observable[A] = a ++ b
    override def point[A](a: => A): Observable[A] = Observable just a
    override def empty[A]: Observable[A] = Observable.empty
  }
}

object MoreObservableInstances extends MoreObservableInstances
