package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichSeq.richSeq
import common.rich.collections.RichTraversableOnce.Rich
import org.scalatest.FlatSpec

class RichSeqTest extends FlatSpec with AuxSpecs {
  "percentage satisfying" should "pass a simple example" in {
    List(2, 4, 5, 6).percentageSatisfying(_ % 2 == 0) should be === 0.75
  }

  "shift" should "do nothing for n = 0" in {
    List(1, 2, 3).shift(0) should be === List(1, 2, 3)
  }

  it should "shift by 1 for n = 1" in {
    List(1, 2, 3).shift(1) should be === List(2, 3, 1)
  }

  it should "shift by 5 for n = 5" in {
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).shift(5) should be === List(6, 7, 8, 9, 10, 1, 2, 3, 4, 5)
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

  "get pairs" should "produce no repeats" in {
    List(1, 2).unorderedPairs.toSet.size should be === 1
  }

  it should "produce pairs" in {
    List(1, 2, 3).unorderedPairs.toSet should be === Set((1, 2), (2, 3), (1, 3))
  }

  "findIndex" should "return the first index matching the predicate" in {
    List(1, 2, 3).findIndex(_ % 2 == 0).get should be === 1
  }

  it should "return None if nothing is found" in {
    List(1, 2, 3).findIndex(_ % 6 == 0) should be === None
  }

  "findWithIndex" should "return the item and the index" in {
    List("foo", "bar", "spam", "eggs").findWithIndex(_.length == 4).get shouldBe("spam", 2)
  }

  it should "return None if nothing is found" in {
    List(1, 2, 3).findWithIndex(_ % 6 == 0) should be === None
  }

  "removeAt" should "return a list with the item removed" in {
    List(1, 2, 3) removeAt 1 should be === List(1, 3)
  }

  it should "throw an exception when the index is too large" in {
    evaluating {
      List(1, 2) removeAt 2
    } should produce[IndexOutOfBoundsException]
  }

  it should "throw an appropriate exception when the index is negative" in {
    evaluating {
      List(1, 2) removeAt -1
    } should produce[IllegalArgumentException]
  }

  it should "Work on edge cases" in {
    List(1) removeAt 0 should be === List()
    List(1, 2) removeAt 1 should be === List(1)
    List(1, 2) removeAt 0 should be === List(2)
  }


  "insert at" should "insert the before the index" in {
    List(1, 3) insert 2 at 1 should be === List(1, 2, 3)
  }

  it should "work in edge cases" in {
    List(1, 3) insert 0 at 0 should be === List(0, 1, 3)
    List(1, 3) insert 4 at 2 should be === List(1, 3, 4)
  }

  it should "work on empty lists" in {
    List[Int]() insert 0 at 0 should be === List(0)
  }

  it should "throw an exception when the index is too large" in {
    evaluating {
      List(1, 2) insert 0 at 3
    } should produce[IndexOutOfBoundsException]
  }

  it should "throw an appropriate exception when the index is negative" in {
    evaluating {
      List(1, 2) insert 0 at -1
    } should produce[IllegalArgumentException]
  }

  "insert after" should "insert after the index" in {
    List(0.0, 1.0) insert 0.5 after 0 should be === List(0.0, 0.5, 1.0)
  }

  it should "throw correct exceptions" in {
    an[IllegalArgumentException] should be thrownBy {List(0) insert 0 after -2} // -1 is fine
    an[IndexOutOfBoundsException] should be thrownBy {List[Int]() insert 0 after 0}
    an[IndexOutOfBoundsException] should be thrownBy {List[Int](0) insert 0 after 1}
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

  "hasSameValues" should "return true when there are the same values" in {
    List((1, "Hi"), (1, "Hello"), (1, "Goodbye")).hasSameValues(_._1) should be === true
  }
  it should "return false when there are different values" in {
    List((1, "Hi"), (1, "Hello"), (2, "Goodbye")).hasSameValues(_._1) should be === false
  }

  "Entropy" should "work for this example that I copied off the net cause I'm a lazy bugger" in {
    Math.round("1223334444".map(_.toInt).entropy * 100000) * 0.00001 should be === 1.84644
  }

  "allUnique" should "work" in {
    List(1, 2).allUnique shouldBe true
    List(1, 1).allUnique shouldBe false
    List(1).allUnique shouldBe true
  }
}
