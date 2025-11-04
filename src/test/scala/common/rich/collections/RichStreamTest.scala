package common.rich.collections

import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import common.rich.collections.RichStream._
import common.test.AuxSpecs

class RichStreamTest extends AnyFreeSpec with AuxSpecs {
  "tailOpt" - {
    "Single element returns None" in {
      Stream(1).tailOption shouldReturn None
    }
    "Empty returns None" in {
      Stream(1).tailOption shouldReturn None
    }
    "Multiple elements returns Some" in {
      Stream(1, 2, 3).tailOption shouldReturn Some(Stream(2, 3))
    }
  }
  "compute" - {
    // The returned buffer will only be changed on computations other than the head.
    def go: (mutable.Buffer[Int], Stream[_]) = {
      var x = -1
      val buffer = new ArrayBuffer[Int]()
      val stream = Stream.continually {
        x += 1
        if (x > 0)
          buffer += x
        ()
      }
      (buffer, stream)
    }
    "0 does nothing" in {
      val (buffer, stream) = go
      stream.compute(0)
      buffer shouldBe 'empty
    }
    "1 does nothing, because head is already strict" in {
      val (buffer, stream) = go
      stream.compute(1)
      buffer shouldBe 'empty
    }
    "5 only computes 4 times" in {
      val (buffer, stream) = go
      stream.compute(5)
      buffer shouldReturn 1.to(4).toBuffer
    }
    "Does not compute extra times" in {
      val (buffer, stream) = go
      stream.compute(5)
      buffer.clear()
      stream.compute(5)
      buffer shouldBe 'empty
    }
  }
}
