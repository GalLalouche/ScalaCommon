package common.rich.collections

import org.scalatest.freespec.AnyFreeSpec

import common.rich.collections.RichJavaStream.richJavaStream
import common.test.AuxSpecs

class RichJavaStreamTest extends AnyFreeSpec with AuxSpecs {
  "toTypedArray" - {
    "empty" in {
      val s = java.util.stream.Stream.empty[String]
      val result: Array[String] = s.toTypedArray
      result shouldBe empty
    }
    "non-empty" in {
      val s = java.util.stream.Stream.of("a", "b", "c")
      val result: Array[String] = s.toTypedArray
      result should contain theSameElementsInOrderAs Seq("a", "b", "c")
    }
  }
}
