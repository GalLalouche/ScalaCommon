package common

import org.scalatest.FreeSpec
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.duration.DurationInt

class AuxSpecsTest extends FreeSpec with AuxSpecs {
  private def doesNotThrow(f: => Unit) {
    f
  }
  private def throws(f: => Unit): TestFailedException = {
    intercept[TestFailedException] {
      f
    }
  }
  private def verifyStackDepth(e: Exception): Unit = {
    val expected = Thread.currentThread().getStackTrace.apply(2)
    val actual = e.getStackTrace.apply(0)
    // This gets mixed up by Scala 2.12 for some reason
    // actual.getMethodName shouldReturn expected.getMethodName
    actual.getLineNumber + 1 shouldReturn expected.getLineNumber
    actual.getFileName shouldReturn expected.getFileName
  }
  private def verifyMessage(e: Exception, message: String): Unit = {
    e.getMessage shouldReturn message
  }
  private def verifyMessageMatches(e: Exception, pattern: String): Unit = {
    e.getMessage matches pattern shouldReturn true
  }
  "shouldReturn" - {
    "ok" in {
      doesNotThrow {
        1 + 1 shouldReturn 2
      }
    }
    "error" in {
      val e = throws(2 + 2 shouldReturn 5)
      verifyStackDepth(e)
      verifyMessage(e, "4 did not equal 5")
    }
  }
  "RichShouldTraversable" - {
    "shouldContain" - {
      "when all is okay" in {
        doesNotThrow(List(1, 2, 3) shouldContain(1, 2, 3))
      }
      "on error" in {
        val e = throws(List(1, 2, 3) shouldContain(1, 2, 4))
        verifyStackDepth(e)
        verifyMessage(e, "List(1, 2, 3) doesn't contain List(4).")
      }
    }
    "shouldSetEqual" - {
      "when all is okay" in {
        doesNotThrow(List(1, 2, 3) shouldSetEqual List(1, 3, 2))
      }
      "on error" - {
        "missing and extra" in {
          val e = throws(List(1, 2, 3) shouldSetEqual List(1, 2, 4))
          verifyStackDepth(e)
          verifyMessage(e,
            "Set(1, 2, 3) isn't the same set as Set(1, 2, 4).\n" +
                "It is missing:       Set(4).\n" +
                "And has extra items: Set(3).")
        }
        "missing only" in {
          val e = throws(List(1, 2) shouldSetEqual List(1, 2, 4))
          verifyStackDepth(e)
          verifyMessage(e,
            "Set(1, 2) isn't the same set as Set(1, 2, 4).\nIt is missing: Set(4).")
        }
        "Extra only" in {
          val e = throws(List(1, 2, 3) shouldSetEqual List(1, 2))
          verifyStackDepth(e)
          verifyMessage(e,
            "Set(1, 2, 3) isn't the same set as Set(1, 2).\nIt has extra items: Set(3).")
        }
      }
    }
    "shouldMultiSetEqual" - {
      "when all is okay" in {
        doesNotThrow(List(1, 2, 3, 2) shouldMultiSetEqual List(1, 3, 2, 2))
      }
      "on error" - {
        "Missing and extra different items" in {
          val e = throws(List(1, 2, 3, 5) shouldMultiSetEqual List(1, 2, 4, 4, 5, 5))
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3, 5) isn't the same MultiSet as MultiSet(1, 2, 4[2], 5[2]).\n" +
                "It is missing:       MultiSet(4[2], 5).\n" +
                "And has extra items: MultiSet(3).")
        }
        "Only extra items" in {
          val e = throws(List(1, 2, 3, 3, 2, 3) shouldMultiSetEqual List(1, 2, 3))
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2[2], 3[3]) isn't the same MultiSet as MultiSet(1, 2, 3).\n" +
                "It has extra items: MultiSet(2, 3[2]).")
        }
        "Only missing items" in {
          val e = throws(List(1, 2, 3) shouldMultiSetEqual List(1, 2, 3, 3, 2, 3))
          verifyStackDepth(e)
          verifyMessage(e,
            "MultiSet(1, 2, 3) isn't the same MultiSet as MultiSet(1, 2[2], 3[3]).\n" +
                "It is missing: MultiSet(2, 3[2]).")
        }
      }
    }
    "allShouldSatisfy" - {
      "when all is okay" in {
        doesNotThrow(List(1, 2, 3) allShouldSatisfy (_ > 0))
      }
      "on error" in {
        val e = throws(List(1, 2, 3, 4, 5) allShouldSatisfy (_ % 2 == 1))
        verifyStackDepth(e)
        verifyMessage(e,
          "Expected all elements in <List(1, 2, 3, 4, 5)> to satisfy the predicate, but <List(2, 4)> don't.")
      }
    }
  }
  "shouldFinish" ignore {
    "finished in time" in {
      {
        1 + 1
      } shouldFinish in lessThan 1.second
    }
    "timing out just by a bit" in {
      val e = throws({
        Thread sleep 10
      } shouldFinish in lessThan 9.milliseconds)
      verifyStackDepth(e)
      verifyMessageMatches(e,
        "Code failed to finish in allotted time \\(\\d+ milliseconds vs\\. 9 milliseconds\\).")
    }
    "timing out by a lot" in {
      val e = throws({
        Thread sleep 10
      } shouldFinish in lessThan 1.milliseconds)
      verifyStackDepth(e)
      verifyMessage(e, "Code failed to finish in allotted time (timed out).")
    }
  }
}
