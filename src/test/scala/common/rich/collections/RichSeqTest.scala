package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichSeq.richSeq
import org.scalatest.FlatSpec

class RichSeqTest extends FlatSpec with AuxSpecs {
  "shift" should "do nothing for n = 0" in {
    List(1, 2, 3).shift(0) shouldReturn List(1, 2, 3)
  }

  it should "shift by 1 for n = 1" in {
    List(1, 2, 3).shift(1) shouldReturn List(2, 3, 1)
  }

  it should "shift by 5 for n = 5" in {
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).shift(5) shouldReturn List(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)
  }

  it should "calculate the modulu shift" in {
    List(1, 2, 3).shift(3) shouldReturn List(1, 2, 3)
  }

  "shifts" should "return all shifts" in {
    List(1, 2, 3).shifts.toSeq shouldReturn Seq(Seq(1, 2, 3), Seq(2, 3, 1), Seq(3, 1, 2))
  }

  it should "return an empty seq for an empty seq" in {
    List().shifts.toSeq shouldReturn Seq()
  }

  "findIndex" should "return the first index matching the predicate" in {
    List(1, 2, 3).findIndex(_ % 2 == 0).get shouldReturn 1
  }

  it should "return None if nothing is found" in {
    List(1, 2, 3).findIndex(_ % 6 == 0) shouldReturn None
  }

  "findWithIndex" should "return the item and the index" in {
    List("foo", "bar", "spam", "eggs").findWithIndex(_.length == 4).get shouldBe("spam", 2)
  }

  it should "return None if nothing is found" in {
    List(1, 2, 3).findWithIndex(_ % 6 == 0) shouldReturn None
  }

  "removeAt" should "return a list with the item removed" in {
    List(1, 2, 3) removeAt 1 shouldReturn List(1, 3)
  }

  it should "throw an exception when the index is too large" in {
    an[IndexOutOfBoundsException] should be thrownBy List(1, 2).removeAt(2)
  }

  it should "throw an appropriate exception when the index is negative" in {
    an[IllegalArgumentException] should be thrownBy List(1, 2).removeAt(-1)
  }

  it should "Work on edge cases" in {
    List(1) removeAt 0 shouldReturn List()
    List(1, 2) removeAt 1 shouldReturn List(1)
    List(1, 2) removeAt 0 shouldReturn List(2)
  }

  "insert at" should "insert the before the index" in {
    List(1, 3) insert 2 at 1 shouldReturn List(1, 2, 3)
  }

  it should "work in edge cases" in {
    List(1, 3) insert 0 at 0 shouldReturn List(0, 1, 3)
    List(1, 3) insert 4 at 2 shouldReturn List(1, 3, 4)
  }

  it should "work on empty lists" in {
    List[Int]() insert 0 at 0 shouldReturn List(0)
  }

  it should "throw an exception when the index is too large" in {
    an[IndexOutOfBoundsException] should be thrownBy List(1, 2).insert(0).at(3)
  }

  it should "throw an appropriate exception when the index is negative" in {
    an[IllegalArgumentException] should be thrownBy List(1, 2).insert(0).at(-1)
  }

  "insert after" should "insert after the index" in {
    List(0.0, 1.0) insert 0.5 after 0 shouldReturn List(0.0, 0.5, 1.0)
  }

  it should "throw correct exceptions" in {
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

  it should "work in edge cases" in {
    List[Int]() insert 0 after -1 shouldReturn List(0)
    List(1) insert 0 after -1 shouldReturn List(0, 1)
  }

  "+" should "append an element at the end" in {
    List[Int]() + 1 shouldBe List(1)
  }

  it should "append an element at the end in non-empty" in {
    List(1) + 2 shouldBe List(1, 2)
  }

  "::" should "prepend to an empty list" in {
    1 :: Seq() shouldBe Seq(1)
  }

  "::" should "prepend to a non-empty list" in {
    1 :: Seq(2) shouldBe Seq(1, 2)
  }
}
