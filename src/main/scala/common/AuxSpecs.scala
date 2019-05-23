package common

import java.io.File
import java.util.concurrent.{Executors, TimeoutException, TimeUnit}

import common.rich.collections.RichTraversableOnce._
import common.rich.primitives.RichBoolean._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, Suite}
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.duration.Duration

/** Several helping methods and fixtures for testing */
trait AuxSpecs extends Matchers {self: Suite =>
  // Since the parameter means jack shit it seems.
  private def throwProperDepthException(message: String, depth: Int): Unit = {
    val e = new TestFailedException(message, 0)
    e.setStackTrace(Thread.currentThread.getStackTrace.drop(depth + 1))
    throw e
  }
  // More information on errors.
  implicit class RichShouldTraversable[T]($: Traversable[T]) {
    private val ProperExceptionDepth = 2
    def shouldContain(first: T, last: T*): Unit = {
      val expected = first :: last.toList
      val missing = expected.filterNot(e => $.exists(_ == e))
      if (missing.nonEmpty)
        throwProperDepthException(s"${$} doesn't contain $missing.", ProperExceptionDepth)
    }

    def shouldSetEqual(other: Traversable[T]): Unit = {
      val otherSet = other.toSet
      val actualSet = $.toSet
      val extra = actualSet &~ otherSet
      val missing = otherSet &~ actualSet
      if (extra.++(missing).nonEmpty)
        throwProperDepthException(
          s"$actualSet isn't the same set as $otherSet.\nIt is missing $missing.\nAnd has extra items: $extra.",
          ProperExceptionDepth)
    }

    private def simplifyMultiSetString[A](xs: Traversable[(A, Int)]): String = xs.map {
      case (x, 1) => x
      case (x, n) => s"$x[$n]"
    }.mkString("MultiSet(", ", ", ")")
    private def diff[A](f1: Map[A, Int], f2: Map[A, Int]): Map[A, Int] = f1.map {
      case (k, v) => k -> (v - f2.getOrElse(k, 0))
    }.filter(_._2 > 0)
    def shouldMultiSetEqual(other: Traversable[T]): Unit = {
      val otherFreqs = other.frequencies
      val actualFreqs = $.frequencies
      val extra = diff(actualFreqs, otherFreqs)
      val missing = diff(otherFreqs, actualFreqs)
      // TODO extract MultiSet to a class?
      if (extra.++(missing).nonEmpty)
        throwProperDepthException(
          s"${simplifyMultiSetString(actualFreqs)} isn't the same MultiSet as ${simplifyMultiSetString(otherFreqs)}.\n"
              + s"It is missing ${simplifyMultiSetString(missing)}.\n"
              + s"And has extra items: ${simplifyMultiSetString(extra)}.",
          ProperExceptionDepth)
    }

    def allShouldSatisfy(p: T => Boolean): Unit = {
      val failingElements = $.filter(p.andThen(_.isFalse))
      if (failingElements.nonEmpty)
        throwProperDepthException(
          s"Expected all elements in <${$}> to satisfy the predicate, but <$failingElements> don't.",
          ProperExceptionDepth)
    }
  }

  // Type-safe equality checking.
  implicit class RichShould[T]($: T) {
    def shouldReturn(t: T) {
      try $ should ===(t)
      catch {
        case e: TestFailedException => throwProperDepthException(e.getMessage, 2)
      }
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
      Some(Duration(System.currentTimeMillis - start, TimeUnit.MILLISECONDS))
    } catch {
      case _: TimeoutException => None
    } finally t.cancel(true)
  }

  /** Usage: {{{ { foobar() } shouldFinish in lessThan 2.seconds. }}} */
  implicit class richBlock(f: => Any) {
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
  implicit def genToArbitrary[A: Gen]: Arbitrary[A] = Arbitrary(implicitly[Gen[A]])
}
