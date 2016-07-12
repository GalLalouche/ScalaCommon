package common

import corpus.ComposablePartialFunctionality
import org.scalatest._

class ComposablePartialFunctionalityTest extends FreeSpec with AuxSpecs {
  "By default" - {
    "An exception should be thrown on apply" in {
      a[MatchError] should be thrownBy {new ComposablePartialFunctionality[Any, Nothing] {}.apply("")}
    }
  }
  "When using ::" - {
    val $ = new ComposablePartialFunctionality[Any, Any] {}

    "It should call the function appended" in {
      var applied = false
      val x: PartialFunction[Any, Unit] = {
        case "Hi there" => applied = true
      }
      (x :: $).apply("Hi there")
      applied shouldBe true;
    }
    "More than once" - {
      "It should use the first" in {
        var count = 0
        val p1: PartialFunction[Any, Any] = {
          case "Hi there" => count += 2
        }
        val p2: PartialFunction[Any, Any] = {
          case "Hi there" => count += 1
        }
        (p1 :: p2 :: $).apply("Hi there")
        count shouldBe 2
      }
      "And if it fails it should use the second" in {
        var count = 0
        val fallBack: PartialFunction[Any, Any] = {
          case "Bar" => count = 1
        }
        val fails: PartialFunction[Any, Any] = {
          case "Foo" => require(false, "should not be called")
        }
        (fails :: fallBack :: $).apply("Bar")
        count shouldBe 1
      }
    }
  }
}
