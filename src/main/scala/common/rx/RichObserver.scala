package common.rx

import rx.lang.scala.Observer

object RichObserver {
  val noop: Observer[Any] = new Observer[Any] {}
}
