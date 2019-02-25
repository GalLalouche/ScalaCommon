package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichSeq.richSeq
import org.scalatest.FreeSpec

class RichSeqTest extends FreeSpec with AuxSpecs {
  "shift" - {
    "does nothing for n = 0" in {
      List(1, 2, 3).shift(0) shouldReturn List(1, 2, 3)
    }

    "shifts by 1 for n = 1" in {
      List(1, 2, 3).shift(1) shouldReturn List(2, 3, 1)
    }

    "shifts by 5 for n = 5" in {
      List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).shift(5) shouldReturn List(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)
    }

    "calculates the modulu shift" in {
      List(1, 2, 3).shift(3) shouldReturn List(1, 2, 3)
    }
  }

  "shifts" - {
    "returns all shifts" in {
      List(1, 2, 3).shifts.toSeq shouldReturn Seq(Seq(1, 2, 3), Seq(2, 3, 1), Seq(3, 1, 2))
    }

    "returns an empty seq for an empty seq" in {
      List().shifts.toSeq shouldReturn Seq()
    }
  }

  "findIndex" - {
    "returns the first index matching the predicate" in {
      List(1, 2, 3).findIndex(_ % 2 == 0).get shouldReturn 1
    }

    "returns None if nothing is found" in {
      List(1, 2, 3).findIndex(_ % 6 == 0) shouldReturn None
    }
  }

  "findWithIndex" - {
    "returns the item and the index" in {
      List("foo", "bar", "spam", "eggs").findWithIndex(_.length == 4).get shouldBe("spam", 2)
    }

    "returns None if nothing is found" in {
      List(1, 2, 3).findWithIndex(_ % 6 == 0) shouldReturn None
    }
  }

  "removeAt" - {
    "returns a list with the item removed" in {
      List(1, 2, 3) removeAt 1 shouldReturn List(1, 3)
    }

    "throws an exception when the index is too large" in {
      an[IndexOutOfBoundsException] should be thrownBy List(1, 2).removeAt(2)
    }

    "throws an appropriate exception when the index is negative" in {
      an[IllegalArgumentException] should be thrownBy List(1, 2).removeAt(-1)
    }

    "works on edge cases" in {
      List(1) removeAt 0 shouldReturn List()
      List(1, 2) removeAt 1 shouldReturn List(1)
      List(1, 2) removeAt 0 shouldReturn List(2)
    }
  }

  "insert at" - {
    "inserts the before the index" in {
      List(1, 3) insert 2 at 1 shouldReturn List(1, 2, 3)
    }

    "works in edge cases" in {
      List(1, 3) insert 0 at 0 shouldReturn List(0, 1, 3)
      List(1, 3) insert 4 at 2 shouldReturn List(1, 3, 4)
    }

    "works on empty lists" in {
      List[Int]() insert 0 at 0 shouldReturn List(0)
    }

    "throws an exception when the index is too large" in {
      an[IndexOutOfBoundsException] should be thrownBy List(1, 2).insert(0).at(3)
    }

    "throws an appropriate exception when the index is negative" in {
      an[IllegalArgumentException] should be thrownBy List(1, 2).insert(0).at(-1)
    }
  }

  "insert after" - {
    "insert after the index" in {
      List(0.0, 1.0) insert 0.5 after 0 shouldReturn List(0.0, 0.5, 1.0)
    }
    "throws correct exceptions" in {
      an[IllegalArgumentException] should be thrownBy {
        List(0) insert 0 after -2
      } // -1 is fine
      an[IndexOutOfBoundsException] should be thrownBy {
        List[Int]() insert 0 after 0
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        List[Int](0) insert 0 after 1
      }
    }
    "works in edge cases" in {
      List[Int]() insert 0 after -1 shouldReturn List(0)
      List(1) insert 0 after -1 shouldReturn List(0, 1)
    }
  }


  "+" - {
    "appends an element at the end" in {
      List[Int]() + 1 shouldBe List(1)
    }

    "appends an element at the end in non-empty" in {
      List(1) + 2 shouldBe List(1, 2)
    }
  }

  "::" - {
    "prepends to an empty list" in {
      1 :: Seq() shouldBe Seq(1)
    }

    "prepends to a non-empty list" in {
      1 :: Seq(2) shouldBe Seq(1, 2)
    }
  }
  "length comparisons" - {
    val list = List(1, 2, 3)
    "checkLength" in {
      list.checkLength(2) shouldReturn RichSeq.Larger
      list.checkLength(3) shouldReturn RichSeq.Equal
      list.checkLength(4) shouldReturn RichSeq.Smaller
    }

    "hasAtLeastSizeOf" in {
      list.hasAtLeastSizeOf(2) shouldReturn true
      list.hasAtLeastSizeOf(3) shouldReturn true
      list.hasAtLeastSizeOf(4) shouldReturn false
    }

    "hasAtMostSizeOf" in {
      list.hasAtMostSizeOf(2) shouldReturn false
      list.hasAtMostSizeOf(3) shouldReturn true
      list.hasAtMostSizeOf(4) shouldReturn true
    }

    "isLargerThan" in {
      list.isLargerThan(2) shouldReturn true
      list.isLargerThan(3) shouldReturn false
      list.isLargerThan(4) shouldReturn false
    }

    "isSmallerThan" in {
      list.isSmallerThan(2) shouldReturn false
      list.isSmallerThan(3) shouldReturn false
      list.isSmallerThan(4) shouldReturn true
    }

    "hasExactlySizeOf" in {
      list.hasExactlySizeOf(2) shouldReturn false
      list.hasExactlySizeOf(3) shouldReturn true
      list.hasExactlySizeOf(4) shouldReturn false
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
}
