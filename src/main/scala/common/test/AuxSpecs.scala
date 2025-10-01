package common.test

import java.io.File
import java.util.concurrent.{Executors, TimeoutException, TimeUnit}

import cats.kernel.CommutativeMonoid
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Assertion, Matchers, Succeeded, Suite}
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.duration.Duration

import common.rich.func.kats.ToMoreMonoidOps.monoidFilter

import common.rich.collections.RichTraversableOnce._

/** Several helping methods and fixtures for testing. */
trait AuxSpecs extends Matchers { self: Suite =>
  // Since the TestFailedException depth parameter means jack shit it seems.
  private def throwIf(cond: Boolean, message: => String, depth: Int): Assertion =
    if (cond) {
      val e = new TestFailedException(message, 0)
      e.setStackTrace(Thread.currentThread.getStackTrace.drop(depth + 1))
      throw e
    } else Succeeded
  private def throwNow(message: String, depth: Int): Assertion =
    throwIf(cond = true, message, depth)
  // More information on errors.
  implicit class RichShouldTraversable[T]($ : Traversable[T]) {
    private val ProperExceptionDepth = 2
    def shouldContain(first: T, rest: T*): Assertion = shouldContainAux(first +: rest)
    def shouldContainAllOf(expected: Traversable[T]): Assertion = shouldContainAux(expected)
    // For consistent stack depth.
    private def shouldContainAux(expected: Traversable[T]): Assertion = {
      val missing = expected.filterNot($.contains)
      throwIf(
        missing.nonEmpty,
        // See https://github.com/scalameta/scalafmt/issues/4456
        s"${this.$} does not contain ${missing.mkString("[", ",", "]")}.",
        ProperExceptionDepth + 1,
      )
    }
    def shouldContainExactly(first: T, rest: T*): Assertion = shouldMultiSetEqualAux(first +: rest)

    private def simplify[A](xs: Traversable[(A, Int)]): String = xs
      .map {
        case (x, 1) => x
        case (x, n) => s"$x[$n]"
      }
      .mkString("MultiSet(", ", ", ")")
    private def diff[A](f1: Map[A, Int], f2: Map[A, Int]): Map[A, Int] = f1
      .map { case (k, v) =>
        k -> (v - f2.getOrElse(k, 0))
      }
      .filter(_._2 > 0)
    def shouldMultiSetEqual(other: Traversable[T]): Assertion = shouldMultiSetEqualAux(other)
    private def shouldMultiSetEqualAux(other: Traversable[T]): Assertion = {
      val otherFreqs = other.frequencies
      val actualFreqs = $.frequencies
      val extra = diff(actualFreqs, otherFreqs)
      val missing = diff(otherFreqs, actualFreqs)
      // TODO extract MultiSet to a class?

      throwIf(
        extra.++(missing).nonEmpty, {
          val base =
            s"${simplify(actualFreqs)} isn't the same as ${simplify(otherFreqs)}."
          val missingStr =
            s"\nIt is missing: ${"      ".monoidFilter(extra.nonEmpty)}${simplify(missing)}."
              .monoidFilter(missing.nonEmpty)
          val extraStr =
            s"\n${if (missing.nonEmpty) "And" else "It"} has extra items: ${simplify(extra)}."
              .monoidFilter(extra.nonEmpty)
          // While it is missing a space, it only exists if missing is empty, so there's nothing to
          // horizontally-align anyway.
          base + missingStr + extraStr
        },
        ProperExceptionDepth + 1,
      )
    }

    def containExactly(t1: T, t2: T, ts: T*): Assertion = shouldMultiSetEqual(t1 :: t2 :: ts.toList)

    def allShouldSatisfy(p: T => Boolean): Assertion = {
      val failingElements = $.filterNot(p)
      throwIf(
        failingElements.nonEmpty,
        // See https://github.com/scalameta/scalafmt/issues/4456
        s"Expected all elements in <${this.$}> to satisfy the predicate, but <$failingElements> don't.",
        ProperExceptionDepth,
      )
    }
  }

  // Type-safe equality checking.
  implicit class RichShould[T]($ : T) {
    def shouldReturn(t: T): Assertion = try $ should ===(t)
    catch {
      case e: TestFailedException => throwNow(message = e.getMessage, depth = 3)
    }
  }

  implicit class RichShouldDouble($ : Double) {
    def shouldBeApproximately(t: Double): Assertion = ($.leftSideValue - t) < 0.000001 shouldBe true
  }

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
    def shouldFinishInLessThan(maxTime: Duration): Assertion = {
      val depth = 7
      getExecutionTime(f, maxTime) match {
        case None =>
          throwNow("Code failed to finish in allotted time (timed out).", depth)
        case Some(actualTime) if actualTime > maxTime =>
          throwNow(s"Code failed to finish in allotted time ($actualTime vs. $maxTime).", depth)
        case _ => Succeeded
      }
    }
  }

  implicit class richAssertion($ : => Assertion) {
    def &&(other: => Assertion): Assertion = assert($ == Succeeded && other == Succeeded)
    def ||(other: => Assertion): Assertion = try
      assert($ == Succeeded)
    catch {
      case _: TestFailedException => other
    }
  }

  def time(f: => Any): Long = {
    val start = System.currentTimeMillis
    f
    System.currentTimeMillis - start
  }

  def getResourceFile(name: String): File = new File(getClass.getResource(name).getFile)
  implicit def genToArbitrary[A: Gen]: Arbitrary[A] = Arbitrary(implicitly[Gen[A]])

  implicit object AssertionMonoid extends CommutativeMonoid[Assertion] {
    override def empty: Assertion = Succeeded
    override def combine(f1: Assertion, f2: Assertion): Assertion = f1 && f2
  }
  def assertAll(assertions: TraversableOnce[Assertion]): Assertion = {
    import cats.syntax.unorderedFoldable.toUnorderedFoldableOps

    import common.rich.func.kats.IteratorInstances.iteratorInstances

    assertions.toIterator.unorderedFold
  }
}
