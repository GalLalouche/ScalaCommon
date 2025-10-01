package common.rich.func.kats

import java.util
import java.util.concurrent.Executors

import cats.{Monad, MonoidK}
import rx.lang.scala.JavaConverters.toScalaScheduler
import rx.lang.scala.Observable
import rx.schedulers.Schedulers

trait ObservableInstances {
  implicit object observableInstances extends Monad[Observable] with MonoidK[Observable] {
    override def flatMap[A, B](fa: Observable[A])(f: A => Observable[B]): Observable[B] =
      fa.flatMap(f)
    override def pure[A](a: A): Observable[A] = Observable.just(a)
    override def empty[A]: Observable[A] = Observable.empty
    override def tailRecM[A, B](initial: A)(f: A => Observable[Either[A, B]]): Observable[B] =
      Observable { subscriber =>
        val queue = new util.LinkedList[Observable[Either[A, B]]]()
        queue.add(f(initial))
        // This is silly. It should use an ExecutionContext or something...
        val scheduler = Schedulers.from(Executors.newSingleThreadExecutor()).asScala
        def next() {
          val polled = queue.poll()
          if (polled == null) {
            subscriber.onCompleted()
            return
          }

          polled
            .to[Vector]
            .subscribe(onNext = { vector =>
              val iterator = queue.listIterator()
              vector.foreach {
                case Left(a) => iterator.add(f(a))
                case Right(b) => subscriber.onNext(b)
              }
              scheduler.createWorker.scheduleRec(next())
            })
        }
        next()
      }
    override def combineK[A](x: Observable[A], y: Observable[A]): Observable[A] = x ++ y
  }
}

object ObservableInstances extends ObservableInstances
