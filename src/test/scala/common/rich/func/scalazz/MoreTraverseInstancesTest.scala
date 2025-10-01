package common.rich.func.scalazz

import org.scalatest.FreeSpec



import scalaz.Traverse
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse.ToTraverseOps
import common.rich.func.scalazz.MoreTraverseInstances._

import common.test.AuxSpecs

class MoreTraverseInstancesTest extends FreeSpec with AuxSpecs {
  private def test[T[_] : Traverse](f: Seq[Int] => T[Int]): Unit = {
    "None" in f(Vector(1, 2, 3)).traverse[Option, Int](i => if (i % 2 == 0) Some(i) else None) shouldReturn None
    "Some" in f(Vector(1, 2, 3)).traverse[Option, Int](Some(_)) shouldReturn Some(Traversable(1, 2, 3))
  }

  "Traverse" - {
    "Seq" - test(_.toSeq)
    "Set" - test(_.toSet)
    "Traversable" - test(_.toTraversable)
    "Iterable" - test(_.toIterable)
  }
}
