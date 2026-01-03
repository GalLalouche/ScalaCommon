package common.rx.report

import rx.lang.scala.Observer

import common.rx.RichObserver

/** A generalization of an [[rx.lang.scala.Observer]] whose onComplete accepts a result value. */
trait ReportObserver[-Agg, -Result] {
  def onStep(a: Agg): Unit
  def onComplete(r: Result): Unit
  def onError(t: Throwable): Unit
  def voidResult(result: => Result): ReportObserver[Agg, Unit] = new ReportObserver[Agg, Unit] {
    override def onStep(a: Agg): Unit = ReportObserver.this.onStep(a)
    override def onComplete(r: Unit): Unit = ReportObserver.this.onComplete(result)
    override def onError(t: Throwable): Unit = ReportObserver.this.onError(t)
  }
}

object ReportObserver {
  val noop: ReportObserver[Any, Any] = from(RichObserver.noop)
  def from[A](observer: Observer[A]): ReportObserver[A, Any] = new ReportObserver[A, Any] {
    override def onStep(a: A): Unit = observer.onNext(a)
    override def onComplete(r: Any): Unit = observer.onCompleted()
    override def onError(t: Throwable): Unit = observer.onError(t)
  }
  def onResult[R](f: R => Unit): ReportObserver[Any, R] = new ReportObserver[Any, R] {
    override def onStep(a: Any): Unit = ()
    override def onComplete(r: R): Unit = f(r)
    override def onError(t: Throwable): Unit = ()
  }
}
