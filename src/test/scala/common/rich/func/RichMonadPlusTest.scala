package common.rich.func

import common.AuxSpecs
import common.rich.func.RichMonadPlus._
import org.scalatest.FreeSpec

import scalaz.std.ListInstances

class RichMonadPlusTest extends FreeSpec with AuxSpecs with ListInstances {
  "select" - {
    "Primitives" - {
      val list = List(3, "Foo", true, 4, 5.0)
      "Any" in {
        list.select[Any] shouldReturn list
      }
      "Int" in {
        list.select[Int] shouldReturn List(3, 4)
      }
      "Double" in {
        list.select[Double] shouldReturn List(5.0)
      }
      "String" in {
        list.select[String] shouldReturn List("Foo")
      }
      "Boolean" in {
        list.select[Boolean] shouldReturn List(true)
      }
      "No type found" in {
        list.select[Char] shouldReturn Nil
      }
      "Nothing" in {
        list.select[Nothing] shouldReturn Nil
      }
    }

    "Custom classes" - {
      trait Parent
      class Child1 extends Parent
      class Child2 extends Parent
      class Child12 extends Child1
      val child1 = new Child1
      val child2 = new Child2
      val child12 = new Child12
      val list = List[Parent](child1, child2, child12)
      "Parent" in {
        list.select[Parent] shouldReturn list
      }
      "Child1" in {
        list.select[Child1] shouldReturn List(child1, child12)
      }
      "Child2" in {
        list.select[Child2] shouldReturn List(child2)
      }
      "Child12" in {
        list.select[Child12] shouldReturn List(child12)
      }
    }
  }
}
