package common.rich.func

import org.scalatest.FreeSpec

import scalaz.std.list.listInstance
import common.rich.func.ToMoreMonadPlusOps._

import common.test.AuxSpecs

class ToMoreMonadPlusOpsTest extends FreeSpec with AuxSpecs {
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
      val list = List[Parent](new Child1, new Child2, new Child12)
      "Parent" in {
        list.select[Parent] shouldReturn list
      }
      "Child1" in {
        list.select[Child1] shouldReturn List(list(0), list(2)).asInstanceOf[List[Child1]]
      }
      "Child2" in {
        list.select[Child2] shouldReturn List(list(1)).asInstanceOf[List[Child2]]
      }
      "Child12" in {
        list.select[Child12] shouldReturn List(list(2)).asInstanceOf[List[Child12]]
      }
    }
  }
  "present" in {
    List(Option("foo"), None).present shouldReturn List("foo")
  }
  "toGuard" in {
    (for {
      x <- List(1, 2, 3)
      _ <- List(x % 2 == 0).toGuard
    } yield x) shouldReturn List(2)
  }
}
