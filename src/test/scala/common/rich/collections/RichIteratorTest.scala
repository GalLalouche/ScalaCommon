package common.rich.collections

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.time.SpanSugar._

import scala.language.postfixOps

import common.rich.collections.RichIterator._
import common.test.AuxSpecs

class RichIteratorTest extends AnyFreeSpec with AuxSpecs with TimeLimitedTests {
  override val timeLimit = 2 seconds
  private def compareIterators[T](actual: Iterator[T], expected: TraversableOnce[T]): Unit =
    actual.toVector shouldReturn expected.toVector

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
      a[NoSuchElementException] shouldBe thrownBy(Iterator.empty.last())
    }
    "returns last" in {
      Iterator(1, 2, 3).last() shouldReturn 3
    }
  }

  "apply" - {
    "throws IndexOutOfBoundsException when index is negative" in {
      an[IndexOutOfBoundsException] shouldBe thrownBy(Iterator.empty.apply(-1))
    }
    "throws IndexOutOfBoundsException when index is too large" in {
      an[IndexOutOfBoundsException] shouldBe thrownBy(Iterator.empty.apply(1))
    }
    "returns element at index 0" in {
      Iterator(1, 2, 3).apply(0) shouldReturn 1
    }
    "returns element at index 1" in {
      Iterator(1, 2, 3).apply(1) shouldReturn 2
    }
    "returns element at index 2" in {
      Iterator(1, 2, 3).apply(2) shouldReturn 3
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
  "headOption" - {
    "returns Some if non-empty" in {
      Iterator(1, 2, 3).headOption() shouldReturn Some(1)
    }
    "throws an exception on empty traversable" in {
      Iterator.empty.headOption() shouldReturn None
    }
  }

  "groupBy" - {
    "groups elements correctly" in {
      Iterator("Apple", "apricot", "banana", "Blueberry", "cherry").groupBy(
        _.head.toLower,
      ) shouldReturn Map(
        'a' -> Vector("Apple", "apricot"),
        'b' -> Vector("banana", "Blueberry"),
        'c' -> Vector("cherry"),
      )
    }
    "handles empty iterator" in {
      (Iterator.empty: Iterator[String]).groupBy[Int](_ => ???) shouldReturn Map.empty
    }
  }
  "sortBy" - {
    "handles empty" in {
      val sorted = (Iterator.empty: Iterator[String]).sortBy[Int](_ => ???)
      sorted shouldReturn Vector.empty
    }
    "sortsBy" in {
      val sorted = Iterator("Apple", "apricot", "banana", "Blueberry", "cherry").sortBy(_.length)
      sorted shouldReturn Vector("Apple", "banana", "cherry", "apricot", "Blueberry")
    }
    "with explicit ordering (mostly verifying compilation doesn't require an implicit class tag)" in {
      val sorted = Iterator("Apple", "apricot", "banana", "Blueberry", "cherry").sortBy(_.length)(
        Ordering.Int.reverse,
      )
      sorted shouldReturn Vector("Blueberry", "apricot", "banana", "cherry", "Apple")
    }
  }

  "object method" - {
    "iterateOptionally" - {
      "Single element" in {
        compareIterators(iterateOptionally(1)(_ => None), Iterator(1))
      }
      "Multiple elements" in {
        compareIterators(
          iterateOptionally(1)(i => if (i < 5) Some(i + 1) else None),
          1.to(5).iterator,
        )
      }
      "Infinite + take" in {
        compareIterators(iterateOptionally(1)(Some apply _ + 1).take(10), 1.to(10).iterator)
      }
    }

    "farthest" - {
      "Returns first element if the function instantly returns None" in {
        farthest(1)(_ => None) shouldReturn 1
      }
      "Returns the last element in a stack safe manner" in {
        farthest(1)(i => if (i < 10000) Some(i + 1) else None) shouldReturn 10000
      }
    }
  }

  "lazyFoldl" - {
    def checkSum(i: Iterator[Int]): Int =
      i.lazyFoldl(1)((x, y) => if (x + y < 10) Some(x * y) else None)
    "empty" in {
      Iterator.empty.lazyFoldl("foo")((_, _) => ???) shouldReturn "foo"
    }
    "finite1" in {
      checkSum(Iterator(1, 2, 3, 4, 5)) shouldReturn 6
    }
    "finite2" in {
      checkSum(Iterator(1, 2)) shouldReturn 2
    }
    "infinite" in {
      checkSum(Iterator.iterate(1)(_ + 1)) shouldReturn 6
    }
  }
}
