package common.rx.report

import rx.lang.scala.Observer

import common.rx.RichObserver

/**
 * A variant of [[Observer]] whose [[Observer.onCompleted]] accepts a result value. Used in
 * conjunction with [[ReportObservable]]
 */
trait ReportObserver[-Agg, -Result] {
  def onNext(a: Agg): Unit
  def onCompleted(r: Result): Unit
  def onError(t: Throwable): Unit
  def voidResult(result: => Result): ReportObserver[Agg, Unit] = new ReportObserver[Agg, Unit] {
    override def onStep(a: Agg): Unit = ReportObserver.this.onStep(a)
    override def onCompleted(r: Unit): Unit = ReportObserver.this.onCompleted(result)
    override def onError(t: Throwable): Unit = ReportObserver.this.onError(t)
  }
}

object ReportObserver {
  val noop: ReportObserver[Any, Any] = from(RichObserver.noop)
  /** Ignores [[ReportObserver#onComplete]]'s `Result` type. */
  def from[A](observer: Observer[A]): ReportObserver[A, Any] = new ReportObserver[A, Any] {
    override def onStep(a: A): Unit = observer.onNext(a)
    override def onCompleted(r: Any): Unit = observer.onCompleted()
    override def onError(t: Throwable): Unit = observer.onError(t)
  }
  /** Ignores [[ReportObserver#onStep]], only listening to the final result. */
  def onResult[R](f: R => Unit): ReportObserver[Any, R] = new ReportObserver[Any, R] {
    override def onStep(a: Any): Unit = ()
    override def onCompleted(r: R): Unit = f(r)
    override def onError(t: Throwable): Unit = ()
  }
}
