package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichMap._
import common.rich.func.MoreSetInstances
import org.scalatest.FreeSpec
import scalaz.std.{ListInstances, StringInstances}

class RichMapTest extends FreeSpec with AuxSpecs with MoreSetInstances with ListInstances with StringInstances {
  "richMap" - {
    "mapKeys" - {
      "no dups" in {
        Map("foo" -> 42, "bazz" -> 54).mapKeys(_.length) shouldReturn Map(3 -> 42, 4 -> 54)
      }
      "dups throws" in {
        an[UnsupportedOperationException] should be thrownBy {Map("foo" -> 2, "bar" -> 3).mapKeys(_.length)}
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
        Map(1 -> "foo", 2 -> "moo").merge(Map(1 -> "bar")) shouldReturn Map(1 -> "foobar", 2 -> "moo")
      }
      "this is smaller" in {
        Map(1 -> "foo").merge(Map(1 -> "bar", 2 -> "moo")) shouldReturn Map(1 -> "foobar", 2 -> "moo")
      }
    }
  }
}
