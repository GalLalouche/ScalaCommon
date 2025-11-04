package common

import org.scalatest.OneInstancePerTest
import org.scalatest.freespec.AnyFreeSpec

import common.test.AuxSpecs

class LazyMapTests extends AnyFreeSpec with AuxSpecs with OneInstancePerTest {
  "applications" - {
    val f = new Function[Int, Int] {
      var count = 0
      override def apply(v1: Int): Int = {
        count += 1
        v1 * 2
      }
    }
    val $ = LazyMap(f).update(12)._2
    "existing values" - {
      "update" - {
        "should return the value if it already exists" in { $.update(12)._1 shouldReturn 24 }
        "f should not be called for a value that already exists" in {
          $.update(12); f.count shouldReturn 1
        }
        "update should return this if the value already exists" in {
          $.update(12)._2 shouldReturn $
        }
      }
      "force" in {
        $.force(12)._2 should not be $
        f.count shouldReturn 2
      }
    }
  }
}
