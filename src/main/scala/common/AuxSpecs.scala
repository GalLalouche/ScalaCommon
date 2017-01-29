package common

import java.io.File
import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

import common.rich.collections.RichSet._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, Suite}

import scala.concurrent.duration.Duration

/** Several helping methods and fixtures for testing */
trait AuxSpecs extends Matchers {
  self: Suite =>

  // since the parameter means jack shit it seems
  private def throwProperDepthException(message: String, depth: Int): Unit = {
    val e = new TestFailedException(message, 0)
    e.setStackTrace(Thread.currentThread.getStackTrace.drop(depth + 1))
    throw e
  }
  // More information on errors
  implicit class RichShouldTraversable[T]($: Traversable[T]) {
    def shouldContain(first: T, last: T*) {
      val expected = first :: last.toList
      val missing = expected.filterNot(e => $.exists(_ == e))
      if (missing.nonEmpty)
        throwProperDepthException(s"${$} doesn't contain $missing.", 2)
    }

    def shouldSetEqual(other: Traversable[T]) {
      val otherSet = other.toSet
      val actualSet = $.toSet
      val extra = actualSet \ otherSet
      val missing = otherSet \ actualSet
      if (extra ++ missing nonEmpty)
        throwProperDepthException(
          s"$actualSet isn't the same set as $otherSet.\nIt is missing $missing.\nAnd has extra items: $extra.", 2)
    }
  }

  // typesafe equality checking
  implicit class RichShould[T]($: T) {
    def shouldReturn(t: T) {
      try $ should be === t
      catch { case e: TestFailedException => throwProperDepthException(e.getMessage, 2) }
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
      Some(System.currentTimeMillis - start).map(Duration(_, TimeUnit.MILLISECONDS))
    } catch {
      case e: TimeoutException => None
    } finally t.cancel(true)
  }

  // usage: { block } shouldFinish in lessThan 2.seconds
  implicit def richBlock(f: => Any) = new {
    def shouldFinish(matcher: NewMatchers) = new {
      def lessThan(maxTime: Duration) = new {
        val depth = 7
        getExecutionTime({
          f
        }, maxTime) match {
          case None =>
            throwProperDepthException("Code failed to finish in allotted time (timed out).", depth)
          case Some(actualTime) if actualTime > maxTime =>
            throwProperDepthException(s"Code failed to finish in allotted time ($actualTime vs. $maxTime).", depth)
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

  def getResourceFile(name: String): File = new File(getClass.getResource(name).getFile)
}