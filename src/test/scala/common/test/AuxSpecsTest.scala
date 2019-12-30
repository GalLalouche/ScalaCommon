package common.test

import org.scalatest.{Assertion, AsyncFreeSpec, Succeeded}
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.duration.DurationInt

class AuxSpecsTest extends AsyncFreeSpec with AuxSpecs {
  private def doesNotThrow(f: => Unit): Assertion = {
    f
    Succeeded
  }
  private def throws(f: => Unit): TestFailedException = intercept[TestFailedException](f)
  private def justThrows(f: => Unit): Assertion = an[TestFailedException] shouldBe thrownBy {f}
  private def verifyStackDepth(e: Throwable): Assertion = {
    val expected = Thread.currentThread().getStackTrace.apply(2)
    val actual = e.getStackTrace.apply(0)
    // This gets mixed up by Scala 2.12 for some reason
    // actual.getMethodName shouldReturn expected.getMethodName
    actual.getLineNumber + 1 shouldReturn expected.getLineNumber
    actual.getFileName shouldReturn expected.getFileName
  }
  private def verifyMessage(e: Exception, message: String): Assertion =
    e.getMessage shouldReturn message

  "shouldReturn" - {
    "ok" in {
      doesNotThrow {1 + 1 shouldReturn 2}
    }
    "error" in {
      val e = throws {2 + 2 shouldReturn 5}
      verifyStackDepth(e)
      verifyMessage(e, "4 did not equal 5")
    }
    "typesafe" in {
      "1 shouldReturn \"foo\"" shouldNot typeCheck
    }
  }

  "RichShouldTraversable" - {
    "shouldContain" - {
      "when all is okay" in {
        doesNotThrow {Vector(1, 2, 3, 4) shouldContain(1, 2, 3)}
      }
      "on error" in {
        val e = throws {Vector(1, 2, 3, 5) shouldContain(1, 2, 4)}
        verifyStackDepth(e)
        verifyMessage(e, "Vector(1, 2, 3, 5) does not contain [4].")
      }
    }
    "shouldContainExactly" - {
      "when all is okay" in {
        doesNotThrow {Vector(1, 2, 3) shouldContainExactly(1, 3, 2)}
      }
      "on error" - {
        "missing and extra" in {
          val e = throws {Vector(1, 2, 3) shouldContainExactly(1, 2, 4)}
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3) isn't the same as MultiSet(1, 2, 4).\n" +
                "It is missing:       MultiSet(4).\n" +
                "And has extra items: MultiSet(3).")
        }
        "missing only" in {
          val e = throws {Vector(1, 2) shouldContainExactly(1, 2, 4)}
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2) isn't the same as MultiSet(1, 2, 4).\n" +
                "It is missing: MultiSet(4).")
        }
        "Extra only" in {
          val e = throws {Vector(1, 2, 3) shouldContainExactly(1, 2)}
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3) isn't the same as MultiSet(1, 2).\n" +
                "It has extra items: MultiSet(3).")
        }
      }
    }
    "shouldMultiSetEqual" - {
      "when all is okay" in {
        doesNotThrow {Vector(1, 2, 3, 2) shouldMultiSetEqual Vector(1, 3, 2, 2)}
      }
      "on error" - {
        "Missing and extra different items" in {
          val e = throws {Vector(1, 2, 3, 5) shouldMultiSetEqual Vector(1, 2, 4, 4, 5, 5)}
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3, 5) isn't the same as MultiSet(1, 2, 4[2], 5[2]).\n" +
                "It is missing:       MultiSet(4[2], 5).\n" +
                "And has extra items: MultiSet(3).")
        }
        "Only extra items" in {
          val e = throws {Vector(1, 2, 3, 3, 2, 3) shouldMultiSetEqual Vector(1, 2, 3)}
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2[2], 3[3]) isn't the same as MultiSet(1, 2, 3).\n" +
                "It has extra items: MultiSet(2, 3[2]).")
        }
        "Only missing items" in {
          val e = throws {Vector(1, 2, 3) shouldMultiSetEqual Vector(1, 2, 3, 3, 2, 3)}
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3) isn't the same as MultiSet(1, 2[2], 3[3]).\n" +
                "It is missing: MultiSet(2, 3[2]).")
        }
      }
    }
    "allShouldSatisfy" - {
      "when all is okay" in {
        doesNotThrow {Vector(1, 2, 3) allShouldSatisfy (_ > 0)}
      }
      "on error" in {
        val e = throws {Vector(1, 2, 3, 4, 5) allShouldSatisfy (_ % 2 == 1)}
        verifyStackDepth(e)
        verifyMessage(e,
          "Expected all elements in <Vector(1, 2, 3, 4, 5)> to satisfy the predicate, but <Vector(2, 4)> don't.")
      }
    }
  }

  "shouldFinish" - {
    "finished in time" ignore {
      {1 + 1} shouldFinishInLessThan 1.second
    }
    "timing out just by a bit" ignore {
      val e = throws {
        {Thread sleep 10} shouldFinishInLessThan 9.milliseconds
      }
      verifyStackDepth(e)
      e.getMessage should fullyMatch regex """Code failed to finish in allotted time \(\d+ milliseconds vs\. 9 milliseconds\)."""
    }
    "timing out by a lot" ignore {
      val e = throws {
        {Thread sleep 10} shouldFinishInLessThan 1.milliseconds
      }
      verifyStackDepth(e)
      verifyMessage(e, "Code failed to finish in allotted time (timed out).")
    }
  }

  "richAssertion" - {
    "&&" - {
      "valid" in {
        doesNotThrow {(1 should ===(1)) && ("foo" should ===("foo"))}
      }
      "lazy" in {
        justThrows {(1 should ===(2)) && ???}
      }
      "invalid right" in {
        justThrows {(1 should ===(1)) && ("foo" should ===("bar"))}
      }
    }
    "||" - {
      "valid" in {
        doesNotThrow {(1 should ===(2)) || ("foo" should ===("foo"))}
      }
      "lazy" in {
        doesNotThrow {(1 should ===(1)) || ???}
      }
      "invalid right" in {
        justThrows {(1 should ===(2)) || ("foo" should ===("bar"))}
      }
    }
  }
}
