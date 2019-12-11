package common

import org.scalatest.FreeSpec
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.duration.DurationInt

class AuxSpecsTest extends FreeSpec with AuxSpecs {
  private def doesNotThrow(f: => Unit) {
    f
  }
  private def throws(f: => Unit): TestFailedException = intercept[TestFailedException](f)
  private def verifyStackDepth(e: Exception): Unit = {
    val expected = Thread.currentThread().getStackTrace.apply(2)
    val actual = e.getStackTrace.apply(0)
    // This gets mixed up by Scala 2.12 for some reason
    // actual.getMethodName shouldReturn expected.getMethodName
    actual.getLineNumber + 1 shouldReturn expected.getLineNumber
    actual.getFileName shouldReturn expected.getFileName
  }
  private def verifyMessage(e: Exception, message: String): Unit =
    e.getMessage shouldReturn message

  "shouldReturn" - {
    "ok" in {
      doesNotThrow {1 + 1 shouldReturn 2}
    }
    "error" in {
      val e = throws(2 + 2 shouldReturn 5)
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
        doesNotThrow(Vector(1, 2, 3) shouldContain(1, 2, 3))
      }
      "on error" in {
        val e = throws(Vector(1, 2, 3) shouldContain(1, 2, 4))
        verifyStackDepth(e)
        verifyMessage(e, "Vector(1, 2, 3) doesn't contain [4].")
      }
    }
    "shouldSetEqual" - {
      "when all is okay" in {
        doesNotThrow(Vector(1, 2, 3) shouldSetEqual Vector(1, 3, 2))
      }
      "on error" - {
        "missing and extra" in {
          val e = throws(Vector(1, 2, 3) shouldSetEqual Vector(1, 2, 4))
          verifyStackDepth(e)
          verifyMessage(e,
            "Set(1, 2, 3) isn't the same set as Set(1, 2, 4).\n" +
                "It is missing:       Set(4).\n" +
                "And has extra items: Set(3).")
        }
        "missing only" in {
          val e = throws(Vector(1, 2) shouldSetEqual Vector(1, 2, 4))
          verifyStackDepth(e)
          verifyMessage(e,
            "Set(1, 2) isn't the same set as Set(1, 2, 4).\nIt is missing: Set(4).")
        }
        "Extra only" in {
          val e = throws(Vector(1, 2, 3) shouldSetEqual Vector(1, 2))
          verifyStackDepth(e)
          verifyMessage(e,
            "Set(1, 2, 3) isn't the same set as Set(1, 2).\nIt has extra items: Set(3).")
        }
      }
    }
    "shouldMultiSetEqual" - {
      "when all is okay" in {
        doesNotThrow(Vector(1, 2, 3, 2) shouldMultiSetEqual Vector(1, 3, 2, 2))
      }
      "on error" - {
        "Missing and extra different items" in {
          val e = throws(Vector(1, 2, 3, 5) shouldMultiSetEqual Vector(1, 2, 4, 4, 5, 5))
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3, 5) isn't the same MultiSet as MultiSet(1, 2, 4[2], 5[2]).\n" +
                "It is missing:       MultiSet(4[2], 5).\n" +
                "And has extra items: MultiSet(3).")
        }
        "Only extra items" in {
          val e = throws(Vector(1, 2, 3, 3, 2, 3) shouldMultiSetEqual Vector(1, 2, 3))
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2[2], 3[3]) isn't the same MultiSet as MultiSet(1, 2, 3).\n" +
                "It has extra items: MultiSet(2, 3[2]).")
        }
        "Only missing items" in {
          val e = throws(Vector(1, 2, 3) shouldMultiSetEqual Vector(1, 2, 3, 3, 2, 3))
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3) isn't the same MultiSet as MultiSet(1, 2[2], 3[3]).\n" +
                "It is missing: MultiSet(2, 3[2]).")
        }
      }
    }
    "allShouldSatisfy" - {
      "when all is okay" in {
        doesNotThrow(Vector(1, 2, 3) allShouldSatisfy (_ > 0))
      }
      "on error" in {
        val e = throws(Vector(1, 2, 3, 4, 5) allShouldSatisfy (_ % 2 == 1))
        verifyStackDepth(e)
        verifyMessage(e,
          "Expected all elements in <Vector(1, 2, 3, 4, 5)> to satisfy the predicate, but <Vector(2, 4)> don't.")
      }
    }
  }

  "shouldFinish" ignore {
    "finished in time" in {
      {1 + 1} shouldFinish in lessThan 1.second
    }
    "timing out just by a bit" in {
      val e = throws {
        {Thread sleep 10} shouldFinish in lessThan 9.milliseconds
      }
      verifyStackDepth(e)
      e.getMessage should fullyMatch regex """Code failed to finish in allotted time \(\d+ milliseconds vs\. 9 milliseconds\)."""
    }
    "timing out by a lot" in {
      val e = throws {
        {Thread sleep 10} shouldFinish in lessThan 1.milliseconds
      }
      verifyStackDepth(e)
      verifyMessage(e, "Code failed to finish in allotted time (timed out).")
    }
  }

  "richAssertion" - {
    "&&" - {
      "valid" in {
        doesNotThrow {
          (1 should ===(1)) && ("foo" should ===("foo"))
        }
      }
      "invalid" in {
        throws {
          (1 should ===(1)) && ("foo" should ===("bar"))
        }
      }
      "lazy" in {
        throws {
          (1 should ===(2)) && ???
        }
      }
    }
    "||" - {
      "valid" in {
        doesNotThrow {
          (1 should ===(2)) || ("foo" should ===("foo"))
        }
      }
      "invalid" in {
        throws {
          (1 should ===(2)) || ("foo" should ===("bar"))
        }
      }
      "lazy" in {
        doesNotThrow {
          (1 should ===(1)) || ???
        }
      }
    }
  }
}
