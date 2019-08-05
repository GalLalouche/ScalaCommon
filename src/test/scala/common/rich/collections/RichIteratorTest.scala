package common.rich.collections

import org.scalatest.FreeSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

import scala.language.postfixOps

import common.AuxSpecs
import common.rich.collections.RichIterator._

class RichIteratorTest extends FreeSpec with AuxSpecs with TimeLimitedTests {
  override val timeLimit = 2 seconds
  private def compareIterators[T](actual: Iterator[T], expected: IterableOnce[T]): Unit =
    actual.toVector shouldReturn expected.iterator.toVector

  "verifyForAll" - {
    "throws an exception if f is not satisfied" in {
      an[Exception] should be thrownBy {
        Iterator(1, 2, 3).verify(_ < 3).toVector
      }
    }

    "doesn't throw an exception if all passes" in {
      compareIterators(Iterator(1, 2, 3).verify(_ < 4), Iterator(1, 2, 3))
    }
  }

  "zipWithIndex zips with index" in {
    compareIterators(Iterator(1.0, 2.0, 3.0).zipWithIndex, Vector((1.0, 0), (2.0, 1), (3.0, 2)))
  }

  "reducingIterator" - {
    "empty" in {
      Iterator[Int]().reducingIterator((_, _) => ???) shouldBe 'empty
    }
    "single element" in {
      compareIterators(Iterator(1).reducingIterator(_ + _), Vector(1))
    }
    "two elements" in {
      compareIterators(Iterator(1, 2).reducingIterator(_ + _), Vector(1, 3))
    }
    "five elements" in {
      compareIterators(Iterator(1, 2, 3, 4, 5).reducingIterator(_ + _), Vector(1, 3, 6, 10, 15))
    }
  }

  "takeUntil" - {
    "empty" in {
      Iterator[Int]().takeUntil(_ => ???) shouldBe 'empty
    }
    "all true" in {
      compareIterators(Iterator(1, 2, 3).takeUntil(_ < 4), Vector(1, 2, 3))
    }
    "first element is false" in {
      compareIterators(Iterator(1, 2, 3).takeUntil(_ != 1), Vector(1))
    }
    "second element is false" in {
      compareIterators(Iterator(1, 2, 3).takeUntil(_ != 2), Vector(1, 2))
    }
  }

  "last" - {
    "throws on empty" in {
      a[NoSuchElementException] shouldBe thrownBy {Iterator.empty.last()}
    }
    "returns last" in {
      Iterator(1, 2, 3).last() shouldReturn 3
    }
  }

  "lastOption" - {
    "None on empty" in {
      Iterator.empty.lastOption() shouldReturn None
    }
    "returns last" in {
      Iterator(1, 2, 3).lastOption() shouldReturn Some(3)
    }
  }
}
