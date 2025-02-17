package common.rich.collections

import java.util

import org.scalatest.{FreeSpec, OneInstancePerTest}

import common.rich.func.BetterSetInstances._
import scalaz.std.string.stringInstance
import scalaz.std.vector.vectorInstance

import common.rich.collections.RichMap._
import common.test.AuxSpecs

class RichMapTest extends FreeSpec with AuxSpecs with OneInstancePerTest {
  "richJavaMap" - {
    val $ : util.Map[String, Integer] = new util.HashMap[String, Integer]()
    $.put("foo", 5)
    "getOrPutIfAbsent" - {
      "exists" in {
        $.getOrPutIfAbsent("foo", ???) shouldReturn 5
        $.get("foo") shouldReturn 5
      }
      "absent" in {
        var first = true
        $.getOrPutIfAbsent(
          "bar",
          if (first) {
            first = false
            6
            // should only be computed once
          } else ???,
        ) shouldReturn 6
        $.get("bar") shouldReturn 6
      }
    }
    "opt" - {
      "exists" in { $.getOpt("foo") shouldReturn Some(5) }
      "absent" in { $.getOpt("bar") shouldReturn None }
    }
  }

  "richMap" - {
    "mapKeys" - {
      "no dups" in {
        Map("foo" -> 42, "bazz" -> 54).mapKeys(_.length) shouldReturn Map(3 -> 42, 4 -> 54)
      }
      "dups throws" in {
        an[UnsupportedOperationException] should be thrownBy {
          Map("foo" -> 2, "bar" -> 3).mapKeys(_.length)
        }
      }
    }
  }

  "richSemigroupMap" - {
    "merge" in {
      Map(1 -> Set("foo"), 2 -> Set("bazz"))
        .merge(Map(1 -> Set("bar"), 3 -> Set("quxx"))) shouldReturn
        Map(1 -> Set("foo", "bar"), 2 -> Set("bazz"), 3 -> Set("quxx"))
    }
    "merge intersecting" in {
      Map(1 -> Set("foo"), 2 -> Set("bazz"))
        .mergeIntersecting(Map(1 -> Set("bar"), 3 -> Set("quxx"))) shouldReturn
        Map(1 -> Set("foo", "bar"))
    }
    "Non abelian" - {
      "this is bigger" in {
        Map(1 -> "foo", 2 -> "moo").merge(Map(1 -> "bar")) shouldReturn Map(
          1 -> "foobar",
          2 -> "moo",
        )
      }
      "this is smaller" in {
        Map(1 -> "foo").merge(Map(1 -> "bar", 2 -> "moo")) shouldReturn Map(
          1 -> "foobar",
          2 -> "moo",
        )
      }
    }
  }

  "richFoldableMap" - {
    val $ = Map("foo" -> Vector(1, 2, 3), "bar" -> Vector(4, 5))
    "flattenValues" in {
      $.flattenValues shouldContainExactly ("foo" -> 1, "foo" -> 2, "foo" -> 3, "bar" -> 4, "bar" -> 5)
    }
    "totalSize" in {
      $.totalSizeSlow shouldReturn 5
      $.totalSize shouldReturn 5
    }
  }
}
