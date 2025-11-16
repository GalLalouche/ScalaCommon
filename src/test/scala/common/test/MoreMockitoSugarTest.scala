package common.test

import org.mockito.Mockito.times
import org.mockito.exceptions.verification.{TooFewActualInvocations, WantedButNotInvoked}
import org.scalatest.OneInstancePerTest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.mockito.MockitoSugar._

import common.test.MoreMockitoSugar._
import common.test.MoreMockitoSugarTest.{Box1, Box2}

class MoreMockitoSugarTest extends AnyFreeSpec with AuxSpecs with OneInstancePerTest {
  private val box2 = mock[Box2]

  "doVerify" - {
    "throws on no calls" in {
      an[WantedButNotInvoked] shouldBe thrownBy(doVerify(box2)(_.foo(Box1(42))))
    }
    "throws on incorrect call count" in {
      box2.foo(Box1(42))
      a[TooFewActualInvocations] shouldBe thrownBy(doVerify(box2, times(2))(_.foo(Box1(42))))
    }
    "No issue when calls are fine" in {
      box2.foo(Box1(42))
      noException shouldBe thrownBy(doVerify(box2)(_.foo(Box1(42))))
    }
  }

  "capture" in {
    box2.foo(Box1(42))
    val captured = capture[Box1](box2)(_.foo(_))
    captured shouldReturn Box1(42)
  }
}

private object MoreMockitoSugarTest {
  case class Box1(x: Int)
  trait Box2 {
    def foo(box1: Box1): Int
  }
}
