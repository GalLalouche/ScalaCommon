package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichMap._
import common.rich.func.MoreSetInstances
import org.scalatest.FreeSpec

import scalaz.std.ListInstances

class RichMapTest extends FreeSpec with AuxSpecs with MoreSetInstances with ListInstances {
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
  }
}
