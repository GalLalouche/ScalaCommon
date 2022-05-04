package common.rich.collections

import org.scalatest.FreeSpec

import common.rich.collections.RichSeq.richSeq
import common.test.AuxSpecs

class RichSeqTest extends FreeSpec with AuxSpecs {
  "shift" - {
    "does nothing for n = 0" in {
      Vector(1, 2, 3).shift(0) shouldReturn Vector(1, 2, 3)
    }

    "shifts by 1 for n = 1" in {
      Vector(1, 2, 3).shift(1) shouldReturn Vector(2, 3, 1)
    }

    "shifts by 5 for n = 5" in {
      Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).shift(5) shouldReturn Vector(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)
    }

    "calculates the modulu shift" in {
      Vector(1, 2, 3).shift(3) shouldReturn Vector(1, 2, 3)
    }
  }

  "shifts" - {
    "returns all shifts" in {
      Vector(1, 2, 3).shifts.toVector shouldReturn Vector(Vector(1, 2, 3), Vector(2, 3, 1), Vector(3, 1, 2))
    }

    "returns an empty seq for an empty seq" in {
      Vector().shifts.toVector shouldReturn Vector()
    }
  }

  "findIndex" - {
    "returns the first index matching the predicate" in {
      Vector(1, 2, 3).findIndex(_ % 2 == 0).get shouldReturn 1
    }

    "returns None if nothing is found" in {
      Vector(1, 2, 3).findIndex(_ % 6 == 0) shouldReturn None
    }
  }

  "findWithIndex" - {
    "returns the item and the index" in {
      Vector("foo", "bar", "spam", "eggs").findWithIndex(_.length == 4).get shouldBe("spam", 2)
    }

    "returns None if nothing is found" in {
      Vector(1, 2, 3).findWithIndex(_ % 6 == 0) shouldReturn None
    }
  }

  "removeAt" - {
    "returns a list with the item removed" in {
      Vector(1, 2, 3) removeAt 1 shouldReturn Vector(1, 3)
    }

    "throws an exception when the index is too large" in {
      an[IndexOutOfBoundsException] should be thrownBy Vector(1, 2).removeAt(2)
    }

    "throws an appropriate exception when the index is negative" in {
      an[IllegalArgumentException] should be thrownBy Vector(1, 2).removeAt(-1)
    }

    "works on edge cases" in {
      Vector(1) removeAt 0 shouldReturn Vector()
      Vector(1, 2) removeAt 1 shouldReturn Vector(1)
      Vector(1, 2) removeAt 0 shouldReturn Vector(2)
    }
  }

  "insert at" - {
    "inserts the before the index" in {
      Vector(1, 3) insert 2 at 1 shouldReturn Vector(1, 2, 3)
    }

    "works in edge cases" in {
      Vector(1, 3) insert 0 at 0 shouldReturn Vector(0, 1, 3)
      Vector(1, 3) insert 4 at 2 shouldReturn Vector(1, 3, 4)
    }

    "works on empty lists" in {
      Vector[Int]() insert 0 at 0 shouldReturn Vector(0)
    }

    "throws an exception when the index is too large" in {
      an[IndexOutOfBoundsException] should be thrownBy Vector(1, 2).insert(0).at(3)
    }

    "throws an appropriate exception when the index is negative" in {
      an[IllegalArgumentException] should be thrownBy Vector(1, 2).insert(0).at(-1)
    }
  }

  "insert after" - {
    "insert after the index" in {
      Vector(0.0, 1.0) insert 0.5 after 0 shouldReturn Vector(0.0, 0.5, 1.0)
    }
    "throws correct exceptions" in {
      an[IllegalArgumentException] should be thrownBy {
        Vector(0) insert 0 after -2
      } // -1 is fine
      an[IndexOutOfBoundsException] should be thrownBy {
        Vector[Int]() insert 0 after 0
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        Vector[Int](0) insert 0 after 1
      }
    }
    "works in edge cases" in {
      Vector[Int]() insert 0 after -1 shouldReturn Vector(0)
      Vector(1) insert 0 after -1 shouldReturn Vector(0, 1)
    }
  }

  "cutoffsAt" - {
    "starts at false" in {
      Vector(1, 7, 2, 3, 4, 5, 6, 6).cutoffsAt(_ % 2 == 0).toVector.map(_.toVector) shouldReturn
          Vector(Vector(1, 7), Vector(2, 3), Vector(4, 5), Vector(6), Vector(6))
    }
    "starts at true" in {
      Vector(6, 4, 2, 3, 4, 5, 6, 6).cutoffsAt(_ % 2 == 0).toVector.map(_.toVector) shouldReturn
          Vector(Vector(6), Vector(4), Vector(2, 3), Vector(4, 5), Vector(6), Vector(6))
    }
  }

  "pairSliding" - {
    "empty returns an empty seq" in {
      Vector().pairSliding shouldBe empty
    }
    "size 1 returns an empty seq" in {
      Vector(1).pairSliding shouldBe empty
    }
    "size 2 returns a seq with a single element" in {
      Vector(1, 2).pairSliding.toVector should ===(Vector(1 -> 2))
    }
    "size n returns the correct seq" in {
      Vector(1, 2, 3, 4).pairSliding.toVector should ===(Vector(1 -> 2, 2 -> 3, 3 -> 4))
    }
  }

  "takeUntilIncluding" - {
    "throw on an empty seq" in {
      a[NoSuchElementException] shouldBe thrownBy {Vector[Int]().takeUntilIncluding(_ => ???)}
    }
    "throw on a sequence contain no element satisfying the predicate" in {
      a[NoSuchElementException] shouldBe thrownBy {Vector(1, 2, 3).takeUntilIncluding(_ < 0)}
    }
    "returns all the elements not satisfying the predicate and first one satisfying it" in {
      Stream.iterate(0)(_ + 1).takeUntilIncluding(_ > 3) shouldReturn Vector(0, 1, 2, 3, 4)
    }
  }

  "safeZipWith" - {
    "fine with empties" in {
      Vector[Int]().safeZipWith(Vector[String]())((_, _) => ???) shouldBe empty
    }
    "throws if left is smaller than right" in {
      val e = the[NoSuchElementException] thrownBy {Vector[Int]().safeZipWith(Vector("foo"))((_, _) => ???)}
      e.getMessage should startWith("Right")
    }
    "throws if left is bigger than right" in {
      val e = the[NoSuchElementException] thrownBy {Vector("foo").safeZipWith(Vector[Int]())((_, _) => ???)}
      e.getMessage should startWith("Left")
    }
    "Returns the map zipped value" in {
      Vector("x", "y", "z").safeZipWith(Vector(1, 2, 0))(_ * _) shouldReturn Vector("x", "yy", "")
    }
  }

  "intersperse" - {
    "empty returns empty" in {
      Vector[Int]().intersperse(???) shouldBe empty
    }
    "Size 1 returns self" in {
      Vector(1).intersperse(???) shouldReturn Vector(1)
    }
    "Places element between each two elements" in {
      var x = 0
      Vector(10, 20, 30).intersperse {
        x += 1
        x
      } shouldReturn Vector(10, 1, 20, 2, 30)
    }
  }

  "orderedGroupBy" in {
    Vector("foo", "bar", "bazz", "y", "moo", "x").orderedGroupBy(_.length) shouldReturn Vector(
      3 -> Vector("foo", "bar", "moo"),
      4 -> Vector("bazz"),
      1 -> Vector("y", "x"),
    )
  }
}
