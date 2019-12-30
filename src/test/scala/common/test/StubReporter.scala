package common.test

import org.scalatest.Reporter
import org.scalatest.events.Event

private object StubReporter extends Reporter {
  override def apply(event: Event): Unit = ()
}
