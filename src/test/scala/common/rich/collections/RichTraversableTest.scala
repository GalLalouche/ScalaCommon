package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichTraversableOnce._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class RichTraversableTest extends FlatSpec with Matchers with AuxSpecs {
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

  "mapDefined" should "only take defined instances" in {
    val f: Any => Option[Int] = {
      case s: String => Some(s.length)
      case _ => None
    }
    List("hi", 4, "hello", true).map(f).flatten shouldReturn List(2, 5)
  }
}
