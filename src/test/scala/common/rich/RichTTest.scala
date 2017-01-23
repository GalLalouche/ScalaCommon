package common.rich

import common.AuxSpecs
import common.rich.RichT._
import org.scalatest.FreeSpec

class RichTTest extends FreeSpec with AuxSpecs {
  "opt" - {
    "None" in {
      val x: Any = null
      x.opt shouldReturn None
    }
    "Some" in { 5.opt shouldReturn Some(5) }
  }
  "only if" - {
    "int" in { 5.onlyIf(false) shouldReturn 0 }
    "double" in { 5.0.onlyIf(false) shouldReturn 0.0 }
    "string" in { "foobar".onlyIf(false) shouldReturn "" }
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
  }
}
