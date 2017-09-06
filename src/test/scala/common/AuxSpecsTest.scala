package common

import org.scalatest.FreeSpec
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.duration.DurationInt

class AuxSpecsTest extends FreeSpec with AuxSpecs {
  private def doesntThrow(f: => Unit) {
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
    //    actual.getMethodName shouldReturn expected.getMethodName
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
      doesntThrow {
        1 + 1 shouldReturn 2
      }
    }
    "error" in {
      val e = throws(2 + 2 shouldReturn 5)
      verifyStackDepth(e)
      verifyMessage(e, "4 did not equal 5")
    }
  }
  "shouldContain" - {
    "when all is okay" in {
      doesntThrow(List(1, 2, 3) shouldContain(1, 2, 3))
    }
    "on error" in {
      val e = throws(List(1, 2, 3) shouldContain(1, 2, 4))
      verifyStackDepth(e)
      verifyMessage(e, "List(1, 2, 3) doesn't contain List(4).")
    }
  }
  "shouldSetEqual" - {
    "when all is okay" in {
      doesntThrow(List(1, 2, 3) shouldSetEqual List(1, 3, 2))
    }
    "on error" in {
      val e = throws(List(1, 2, 3) shouldSetEqual List(1, 2, 4))
      verifyStackDepth(e)
      verifyMessage(e,
        "Set(1, 2, 3) isn't the same set as Set(1, 2, 4).\nIt is missing Set(4).\nAnd has extra items: Set(3).")
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
