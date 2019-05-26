package common.rich

import common.AuxSpecs
import common.rich.RichT._
import org.scalatest.{FreeSpec, Matchers}

class RichTTest extends FreeSpec with AuxSpecs with Matchers {
  "tryOrKeep" - {
    "succeeds" in {
      5.tryOrKeep(_ + 1) shouldReturn 6
    }
    "fails" in {
      5.tryOrKeep(_ => ???) shouldReturn 5
    }
  }

  "mapIf" - {
    "predicate" - {
      "true" in {5.mapIf(_ > 5).to(6) shouldReturn 5}
      "false" in {5.mapIf(_ < 10).to(_ * 5) shouldReturn 25}
    }
    "boolean" - {
      "true" in {5.mapIf(false).to(_ * 5) shouldReturn 5}
      "false" in {5.mapIf(true).to(10) shouldReturn 10}
    }
  }

  "opt" - {
    "None" in {
      val x: Any = null
      x.opt shouldReturn None
    }
    "Some" in {
      5.opt shouldReturn Some(5)
    }
  }
  "optFilter" - {
    "Null" in {
      val x: Any = null
      x.optFilter(_ => ???) shouldReturn None
    }
    "None" in {
      4.optFilter(_ % 2 != 0) shouldReturn None
    }
    "Some" in {
      4.optFilter(_ % 2 != 4) shouldReturn Some(4)
    }
  }

  "only if" - {
    "int" - {
      "false" in {
        5.onlyIf(false) shouldReturn 0
      }
      "true" in {
        5.onlyIf(true) shouldReturn 5
      }
    }
    "double" - {
      "false" in {
        5.0.onlyIf(false) shouldReturn 0.0
      }
      "true" in {
        5.0.onlyIf(true) shouldReturn 5.0
      }
    }
    "string" - {
      "false" in {
        "foobar".onlyIf(false) shouldReturn ""
      }
      "true" in {
        "foobar".onlyIf(true) shouldReturn "foobar"
      }
    }
    "classes" - {
      "false" in {
        List(1, 2, 3).onlyIf(false) shouldReturn null
      }
      "true" in {
        List(1, 2, 3).onlyIf(true) shouldReturn List(1, 2, 3)
      }
    }
  }

  "safe cast" - {
    "primitives" - {
      "Any" in {
        val $: Any = 3
        $.safeCast[Any] shouldReturn Some(3)
      }
      "Int" in {
        3.safeCast[Int] shouldReturn Some(3)
      }
      "Double" in {
        3.0.safeCast[Double] shouldReturn Some(3.0)
      }
      "String" in {
        "foo".safeCast[String] shouldReturn Some("foo")
      }
      "Boolean" in {
        true.safeCast[Boolean] shouldReturn Some(true)
      }
      "Nothing" in {
        3.safeCast[Nothing] shouldReturn None
      }
    }
    "type errors" - {
      "types don't match" in {
        "common.rich.RichT.richT(true).safeCast[Int]" shouldNot typeCheck
      }
      "safeCast to parent" in {
        // Safe casting to a parent isn't needed. If C <: P, then C is already a P.
        "common.rich.RichT.richT(3).safeCast[AnyVal]" shouldNot typeCheck
      }
    }
    "classes" - {
      "inheritance" in {
        class A
        class B extends A
        val b: A = new B
        b.safeCast[B] should ===(Some(b))
      }
    }
  }

  "safeAs" - {
    trait Speaks
    class SuperType
    case class Person(name: String) extends Speaks
    class Dog
    "not a returns None" in {
      new Dog().safeAs[Speaks] shouldReturn None
    }
    "is a returns Some" in {
      val person = Person("foobar")
      person.safeAs[Speaks] shouldReturn Some(person)
    }
  }
  "lazyT" - {
    "const" - {
      "returns t" in {
        5.const.apply("foobar") shouldReturn 5
      }
      "is lazy" in {
        def foo: Int = ???
        None.map(foo.const) shouldReturn None
      }
    }
    "partialConst" - {
      "returns t" in {
        5.partialConst.apply("foobar") shouldReturn 5
      }
      "is lazy" in {
        def foo: Int = ???
        None.map(foo.partialConst) shouldReturn None
      }
    }
  }

  "anyRefT" - {
    "neq" - {
      "true" in {
        "foo" neq "bar" shouldReturn true
      }
      "false" in {
        "foo" neq "foo" shouldReturn false
      }
      "super type" in {
        val x: AnyRef = Nil
        val y = "bar"
        x neq y shouldReturn true
      }
      "doesn't compile invalid types" in {
        """Nil.neq("foo")""" shouldNot typeCheck
      }
    }
  }
}
