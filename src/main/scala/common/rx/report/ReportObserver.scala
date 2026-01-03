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
  /**
   * Returns an [[Observer]] that when completed will call [[ReportObserver.onCompleted]] with the
   * given `result`.
   */
  def toObserver(result: => Result): Observer[Agg] = new Observer[Agg] {
    override def onNext(a: Agg): Unit = ReportObserver.this.onNext(a)
    override def onCompleted(): Unit = ReportObserver.this.onCompleted(result)
    override def onError(t: Throwable): Unit = ReportObserver.this.onError(t)
  }
}

object ReportObserver {
  val noop: ReportObserver[Any, Any] = from(RichObserver.noop)
  /** Ignores [[ReportObserver#onComplete]]'s `Result` type. */
  def from[A](observer: Observer[A]): ReportObserver[A, Any] = new ReportObserver[A, Any] {
    override def onNext(a: A): Unit = observer.onNext(a)
    override def onCompleted(r: Any): Unit = observer.onCompleted()
    override def onError(t: Throwable): Unit = observer.onError(t)
  }
  /** Ignores [[ReportObserver#onStep]], only listening to the final result. */
  def onResult[R](f: R => Unit): ReportObserver[Any, R] = new ReportObserver[Any, R] {
    override def onNext(a: Any): Unit = ()
    override def onCompleted(r: R): Unit = f(r)
    override def onError(t: Throwable): Unit = ()
  }
  /**
   * Listens to [[ReportObserver#onStep]] and returns the final result. If
   * [[ReportObserver#onError]] was called, the error will be thrown instead.
   */
  def asReturnValue[Agg, Result <: AnyRef](
      f: ReportObserver[Agg, Result] => Unit,
  )(obs: Observer[Agg]): Result = {
    var result: Result = null.asInstanceOf[Result]
    var error: Throwable = null
    f(new ReportObserver[Agg, Result] {
      override def onNext(a: Agg): Unit = obs.onNext(a)
      override def onCompleted(r: Result): Unit = result = r
      override def onError(t: Throwable): Unit = {
        obs.onError(t)
        error = t
      }
    })
    if (error != null)
      throw error
    result.ensuring(_ != null)
  }
}
