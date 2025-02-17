package common.rich.func.scalazz

import org.scalatest.FreeSpec

import common.rich.func.scalazz.ToMoreMonadPlusOps.toMoreMonadPlusOps
import scalaz.OptionT
import scalaz.std.vector.vectorInstance

import common.test.AuxSpecs

class ToMoreMonadPlusOpsTest extends FreeSpec with AuxSpecs {
  "select" - {
    "Primitives" - {
      val list = Vector(3, "Foo", true, 4, 5.0)
      "Any" in {
        list.select[Any] shouldReturn list
      }
      "Int" in {
        list.select[Int] shouldReturn Vector(3, 4)
      }
      "Double" in {
        list.select[Double] shouldReturn Vector(5.0)
      }
      "String" in {
        list.select[String] shouldReturn Vector("Foo")
      }
      "Boolean" in {
        list.select[Boolean] shouldReturn Vector(true)
      }
      "No type found" in {
        list.select[Char] shouldBe empty
      }
      "Nothing" in {
        list.select[Nothing] shouldBe empty
      }
    }

    "Custom classes" - {
      trait Parent
      class Child1 extends Parent
      class Child2 extends Parent
      class Child12 extends Child1
      val list = Vector[Parent](new Child1, new Child2, new Child12)
      "Parent" in {
        list.select[Parent] shouldReturn list
      }
      "Child1" in {
        list.select[Child1] shouldReturn Vector(list(0), list(2)).asInstanceOf[Vector[Child1]]
      }
      "Child2" in {
        list.select[Child2] shouldReturn Vector(list(1)).asInstanceOf[Vector[Child2]]
      }
      "Child12" in {
        list.select[Child12] shouldReturn Vector(list(2)).asInstanceOf[Vector[Child12]]
      }
    }
  }
  "present" in {
    Vector(Option("foo"), None).present shouldReturn Vector("foo")
  }
  "toGuard" in {
    (for {
      x <- Vector(1, 2, 3)
      _ <- Vector(x % 2 == 0).toGuard
    } yield x) shouldReturn Vector(2)
  }
  "withFilter" - {
    val monad = OptionT.some[Vector, String]("foobar")
    "true" in {
      val $ = for {
        m <- monad
        if m.length == 6
      } yield m
      $.run shouldReturn Vector(Some("foobar"))
    }
    "false" in {
      val $ = for {
        m <- monad
        if m.length == 5
      } yield m
      $.run shouldReturn Vector(None)
    }
  }
}
