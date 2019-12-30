package common.rich.collections

import org.scalatest.FreeSpec
import org.scalatest.OptionValues._

import scala.collection.mutable.ArrayBuffer

import scalaz.Semigroup

import common.rich.collections.RichTraversableOnce._
import common.test.AuxSpecs

class RichTraversableOnceTest extends FreeSpec with AuxSpecs {
  "filterNot" in {
    1.to(5).filterNot(_ % 2 == 0).toVector shouldReturn Vector(1, 3, 5)
  }
  "unorderedPairs" - {
    "has size of n choose 2" in {
      Vector.fill(10)("bla").unorderedPairs.size shouldReturn 45
    }

    "returns all unique pairs" in {
      Vector(1, 2, 3).unorderedPairs.toSet shouldReturn Set((1, 2), (1, 3), (2, 3))
    }
  }

  "joinWhere" - {
    "returns an empty traversable if one of the traversables is empty" in {
      Vector(1, 2, 3).join(Vector[String]()).where((_, _) => true).toVector shouldBe 'empty
      Vector[Int]().join(Vector[String]("1", "2", "3")).where((_, _) => true).toVector shouldBe 'empty
    }

    "returns a cartesian product if the 'where' function is true" in {
      Vector(1, 2, 3).join(Vector(4, 5, 6)).where((_, _) => true).toSet.size shouldBe 9
    }

    "filters by the where function" in {
      Vector(1, 2).join(Vector(4, 5)).where((e, f) => e * f % 2 == 0).toSet shouldBe Set((2, 4), (1, 4), (2, 5))
    }
  }

  private case class Person(name: String, id: Long)
  private case class Grade(id: Long, grade: Int)
  private case class PersonWithGrade(name: String, id: Long, grade: Int)

  "joinBy" - {
    "joins two items whose extractor returns the same value" in {
      Vector(Person("gal", 1), Person("noam", 2)).join(Vector(Grade(1, 100), Grade(2, 0))).by(_.id, _.id).toVector shouldBe
          Vector((Person("gal", 1), Grade(1, 100)), (Person("noam", 2), Grade(2, 0)))
    }

    "creates a new object if such a builder is provided " in {
      Vector(Person("gal", 1), Person("noam", 2))
          .join(Vector(Grade(1, 100), Grade(2, 0)))
          .by(_.id, _.id, (p, g) => PersonWithGrade(p.name, p.id, g.grade))
          .toSet shouldBe Set(PersonWithGrade("gal", 1, 100), PersonWithGrade("noam", 2, 0))
    }
  }

  "foreachWithInBetween performs the selectedActionInBetween each foreach" in {
    val actual = ArrayBuffer[String]()
    Vector(1, 2, 3).foreachWithBetween(actual += _.toString, () => actual += "|")
    actual.mkString("") shouldBe "1|2|3"
  }

  "selectRepresentative selects a representative from each class" in {
    (1 to 100).selectRepresentative(_ % 5).map(_ % 5).toSet.size shouldBe 5
  }

  "allUnique" in {
    Vector(1, 2).allUnique shouldBe true
    Vector(1, 1).allUnique shouldBe false
    Vector(1).allUnique shouldBe true
  }

  "hasSameValues" - {
    "returns true when there are the same values" in {
      Vector((1, "Hi"), (1, "Hello"), (1, "Goodbye")).hasSameValues(_._1) shouldReturn true
    }
    "returns false when there are different values" in {
      Vector((1, "Hi"), (1, "Hello"), (2, "Goodbye")).hasSameValues(_._1) shouldReturn false
    }
  }

  "Entropy works for this example that I copied off the net cause I'm a lazy bugger" in {
    Math.round("1223334444".map(_.toInt).entropy * 100000) * 0.00001 shouldReturn 1.84644
  }

  "unorderedPairs" - {
    "produces no repeats" in {
      Vector(1, 2).unorderedPairs.toSet.size shouldReturn 1
    }

    "produces pairs" in {
      Vector(1, 2, 3).unorderedPairs.toSet shouldReturn Set((1, 2), (2, 3), (1, 3))
    }
  }

  "percentage satisfying passes a simple example" in {
    Vector(2, 4, 5, 6).percentageSatisfying(_ % 2 == 0) shouldReturn 0.75
  }

  "single" - {
    "returns the single element when one exists" in {
      Vector(1).single shouldReturn 1
    }
    "throws an exception on empty traversable" in {
      a[NoSuchElementException] should be thrownBy Nil.single
    }
    "throws an exception on a traversable with more than 1 element" in {
      an[UnsupportedOperationException] should be thrownBy Vector(1, 2).single
    }
  }
  "singleOpt" - {
    "returns the single element when one exists" in {
      Vector(1).singleOpt shouldReturn Some(1)
    }
    "throws an exception on empty traversable" in {
      Nil.singleOpt shouldReturn None
    }
    "throws an exception on a traversable with more than 1 element" in {
      an[UnsupportedOperationException] should be thrownBy Vector(1, 2).single
    }
  }

  "mapFirst" - {
    "empty returns None" in {
      Vector[Int]().mapFirst(_ => Some(1)) shouldReturn None
    }
    "If all None, returns None" in {
      Vector(1, 2, 3).mapFirst(_ => None) shouldReturn None
    }
    "Returns first Some" in {
      Iterator.iterate(1)(_ + 1).mapFirst(e => if (e > 10) Some((e * e).toString) else None)
          .value shouldReturn "121"
    }
  }

  "mapBy" - {
    "maps by the function" in {
      Vector("Hello", "world!").mapBy(_.length) shouldReturn Map(5 -> "Hello", 6 -> "world!")
    }
    "throws on repeats" in {
      an[IllegalArgumentException] should be thrownBy Vector(1, 2).mapBy(_.toString.length)
    }
  }

  "toMultimap" - {
    "creates a multimap" in {
      Vector("one", "two", "three").toMultiMap(_.length) shouldReturn Map(3 -> Vector("one", "two"), 5 -> Vector("three"))
    }
    "extracts the value when passed" in {
      Vector("one", "two", "three").toMultiMap(_.length, _.toUpperCase) shouldReturn Map(3 -> Vector("ONE", "TWO"), 5 -> Vector("THREE"))
    }
  }

  "aggregateMap" in {
    implicit val concatStrings: Semigroup[String] = Semigroup.instance(_ + _)
    Vector("one", "two", "three").aggregateMap(_.length, _.toUpperCase) shouldReturn Map(3 -> "ONETWO", 5 -> "THREE")
  }

  "reduceByKey" in {
    implicit val concatStrings: Semigroup[String] = Semigroup.instance(_ + _)
    Vector("one", "two", "three").reduceByKey(_.length) shouldReturn Map(3 -> "onetwo", 5 -> "three")
  }

  "filterAndSortBy" - {
    "returns an empty seq on empty" in {
      Vector(1, 2, 3).filterAndSortBy(identity, Nil) shouldReturn Nil
    }
    "filters and sort by" in {
      Vector("one", "four", "three").filterAndSortBy(_.length, Vector(5, 3)) shouldReturn Vector("three", "one")
    }
  }

  "product works on iterators" in {
    val $ = Iterator(1, 2, 3) * Iterator(4, 5, 6)
    $.toVector shouldReturn Vector(1 -> 4, 1 -> 5, 1 -> 6, 2 -> 4, 2 -> 5, 2 -> 6, 3 -> 4, 3 -> 5, 3 -> 6)
  }

  "fornone" - {
    "true" in {Vector(1, 2, 3).fornone(_ == 4) shouldReturn true}
    "false" in {Vector(1, 2, 3).fornone(_ == 3) shouldReturn false}
  }
  "existsNot" - {
    "true" in {Vector(1, 2, 3).existsNot(_ == 3) shouldReturn true}
    "false" in {Vector(1, 2, 3).existsNot(_ != 4) shouldReturn false}
  }

  "range" in {
    Iterator(1, 2, 3).range shouldReturn(1, 3)
  }
}
