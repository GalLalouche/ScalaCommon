package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichTraversableOnce._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class RichTraversableOnceTest extends FlatSpec with Matchers with AuxSpecs {
  "unorderedPairs" should "have size of n choose 2" in {
    List.fill(10)("bla").unorderedPairs.size should be === 45
  }

  it should "return all unique pairs" in {
    List(1, 2, 3).unorderedPairs.toSet should be === Set((1, 2), (1, 3), (2, 3))
  }

  "joinWhere" should "return an empty traversable if one of the traversables is empty" in {
    List(1, 2, 3).join(List[String]()).where((e, f) => true) shouldBe Traversable()
    List().join(List[String]("1", "2", "3")).where((e, f) => true) shouldBe Traversable()
  }

  it should "return a cartesian product if the 'where' function is true" in {
    List(1, 2, 3).join(List(4, 5, 6)).where((e, f) => true).toSet.size shouldBe 9
  }

  it should "filter by the where function" in {
    List(1, 2).join(List(4, 5)).where((e, f) => e * f % 2 == 0).toSet shouldBe Set((2, 4), (1, 4), (2, 5))
  }

  private case class Person(name: String, id: Long)

  private case class Grade(id: Long, grade: Int)

  private case class PersonWithGrade(name: String, id: Long, grade: Int)

  "joinBy" should "join two items whose extractor returns the same value" in {
    List(Person("gal", 1), Person("noam", 2)) join List(Grade(1, 100), Grade(2, 0)) by(_.id, _.id) shouldBe
      List((Person("gal", 1), Grade(1, 100)), (Person("noam", 2), Grade(2, 0)))
  }

  it should "create a new object if such a builder is provided " in {
    List(Person("gal", 1), Person("noam", 2))
      .join(List(Grade(1, 100), Grade(2, 0)))
      .by(_.id, _.id, (p, g) => PersonWithGrade(p.name, p.id, g.grade))
      .toSet shouldBe Set(PersonWithGrade("gal", 1, 100), PersonWithGrade("noam", 2, 0))
  }

  "foreachWithInBetween" should "Perform the selectedActionInBetween each foreach" in {
    val actual = mutable.MutableList[String]()
    List(1, 2, 3).foreachWithBetween(actual += _.toString, () => actual += "|")
    actual.mkString("") shouldBe "1|2|3"
  }

  "selectRepresentative" should "Select a representative from each class" in {
    (1 to 100).selectRepresentative(_ % 5).map(_ % 5).toSet.size shouldBe 5
  }

  "allUnique" should "work" in {
    List(1, 2).allUnique shouldBe true
    List(1, 1).allUnique shouldBe false
    List(1).allUnique shouldBe true
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

  "get pairs" should "produce no repeats" in {
    List(1, 2).unorderedPairs.toSet.size should be === 1
  }

  it should "produce pairs" in {
    List(1, 2, 3).unorderedPairs.toSet should be === Set((1, 2), (2, 3), (1, 3))
  }

  "percentage satisfying" should "pass a simple example" in {
    List(2, 4, 5, 6).percentageSatisfying(_ % 2 == 0) should be === 0.75
  }

}
