package common

import java.util.concurrent._

import org.scalatest._

import scala.concurrent.duration._

/** Several helping methods and fixtures for testing */
trait AuxSpecs extends Matchers {
	self: Suite =>

	implicit class RichShould[T]($: T) {
		def shouldReturn(t: T) {
			$ shouldBe t
		}
	}
	
	implicit class RichShouldDouble($: Double) {
		def shouldBeApproximately(t: Double) {
			($.leftSideValue - t) < 0.000001 shouldBe true
		}
	}
	sealed abstract class NewMatchers

	case object in extends NewMatchers

	private lazy val executors = Executors.newFixedThreadPool(10)
	private def getExecutionTime(f: => Any, maxTime: Duration): Option[Duration] = {
		val t = executors.submit(new Runnable() {
			override def run() {
				f
			}
		})
		val start = System.currentTimeMillis
		try {
			t.get(2 * maxTime.toMillis, TimeUnit.MILLISECONDS)
			Some(System.currentTimeMillis - start).map(_.milliseconds)
		} catch {
			case e: TimeoutException => None
		} finally t.cancel(true)
	}

	implicit def richBlock(f: => Any) = new {
		def shouldFinish(matcher: NewMatchers) = new {
			def lessThan(maxTime: Duration) = new {
				getExecutionTime({f}, maxTime) match {
					case None =>
						throw new TestFailedException("Code failed to finish in allotted time (timed out)", 6)
					case Some(actualTime) if actualTime > maxTime =>
						throw new TestFailedException(s"Code failed to finish in allotted time ($actualTime vs $maxTime)", 6)
					case _ => ()
				}
			}
		}
	}

	def time(f: => Any): Long = {
		val start = System.currentTimeMillis
		f
		System.currentTimeMillis - start
	}
}
