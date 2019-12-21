package common

import org.scalatest.Reporter
import org.scalatest.events.Event

object StubReporter extends Reporter {
  override def apply(event: Event): Unit = ()
}
