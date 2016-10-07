package common.rich.func

import common.AuxSpecs
import common.rich.func.MoreTraverse._
import org.scalatest.FreeSpec

import scalaz.std.OptionInstances
import scalaz.syntax.ToTraverseOps

class MoreTraverseTest extends FreeSpec with AuxSpecs with OptionInstances with ToTraverseOps {
  "TraversableTraverse" - {
    "None" in {
      Traversable(1, 2, 3).traverse[Option, Int](i => if (i % 2 == 0) Some(i) else None) shouldReturn None
    }
    "Some" in {
      Traversable(1, 2, 3).traverse[Option, Int](Some(_)) shouldReturn Some(Traversable(1, 2, 3))
    }
  }
}
