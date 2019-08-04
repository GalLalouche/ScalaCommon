package common.rich.func

import org.scalatest.FreeSpec

import scala.language.higherKinds

import scalaz.Traverse
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse._
import common.rich.func.MoreTraverseInstances._

import common.AuxSpecs

class MoreTraverseInstancesTest extends FreeSpec with AuxSpecs {
  private def test[T[_] : Traverse](f: Seq[Int] => T[Int]): Unit = {
    "None" in f(List(1, 2, 3)).traverse[Option, Int](i => if (i % 2 == 0) Some(i) else None) shouldReturn None
    "Some" in f(List(1, 2, 3)).traverse[Option, Int](Some(_)) shouldReturn Some(Traversable(1, 2, 3))
  }

  "Traverse" - {
    "Seq" - test(_.toSeq)
    "Set" - test(_.toSet)
    "Traversable" - test(_.toTraversable)
    "Iterable" - test(_.toIterable)
  }
}
