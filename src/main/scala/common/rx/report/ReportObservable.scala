package common.rx.report

import cats.implicits.toFunctorOps
import rx.lang.scala.{Observable, Observer, Subscription}

import common.rich.func.kats.ObservableInstances.observableInstances

import common.rich.primitives.RichBoolean.richBoolean

/** A variant of [[Observable]] that can update [[ReportObserver]]. */
object ReportObservable {
  type ReportObservable[Agg, Result] = Function[ReportObserver[Agg, Result], Subscription]
  def aggregator[Agg, Result](
      observable: Observable[Agg],
      finisher: Seq[Agg] => Result,
  ): ReportObservable[Agg, Result] = filteringAggregator(observable.tupleLeft(true), finisher)

  def void[A](observable: Observable[A]): ReportObservable[A, Unit] = observer =>
    observable.subscribe(
      a => observer.onNext(a),
      t => observer.onError(t),
      () => observer.onCompleted(()),
    )

  type ShouldNotify = Boolean
  def filteringAggregator[Agg, Result](
      observable: Observable[(ShouldNotify, Agg)],
      finisher: Seq[Agg] => Result,
  ): ReportObservable[Agg, Result] = apply(
    Observable[Report[Agg, Result]](s =>
      observable
        .doOnError(s.onError)
        .foldLeft(Vector.newBuilder[Agg]) { case (buffer, (shouldNotify, next)) =>
          if (shouldNotify)
            s.onNext(Aggregation(next))
          buffer.synchronized {
            buffer += next
          }
        }
        .map(_.result())
        .subscribe(buffer => s.onNext(Result(finisher(buffer)))),
    ),
  )
  /**
   * Creates a [[ReportObservable]] from an [[Observable]] that can output either `Agg` or `Result`.
   * The `observable` should output `Result` at most once, at which point the subscribed
   * [[ReportObserver.onCompleted]] will be called. The `observable` may also invoke
   * [[Observer.onCompleted]], but only after emitting a `Result`.
   */
  def apply[Agg, Result](o: Observable[Report[Agg, Result]]): ReportObservable[Agg, Result] = ro =>
    o.subscribe(new Observer[Report[Agg, Result]] {
      // Everything is synchronized, so this doesn't need to be atomic.
      private var hasCompleted = false
      private var hasErrored = false
      override def onNext(value: Report[Agg, Result]) = synchronized {
        value match {
          case Aggregation(value) =>
            require(hasCompleted.isFalse, "Aggregation emitted after Result")
            require(hasErrored.isFalse)
            ro.onNext(value)
          case Result(value) =>
            require(hasCompleted.isFalse, "Result emitted multiple times")
            require(hasErrored.isFalse)
            hasCompleted = true
            ro.onCompleted(value)
        }
      }
      override def onError(error: Throwable) = synchronized {
        require(hasCompleted.isFalse)
        require(hasErrored.isFalse)
        hasErrored = true
        ro.onError(error)
      }
      override def onCompleted() = synchronized {
        require(hasCompleted, "onCompleted called before Result was emitted")
        require(hasErrored.isFalse)
      }
    })
}
