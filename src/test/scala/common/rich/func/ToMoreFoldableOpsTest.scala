package common.rich.func

import common.AuxSpecs
import org.scalatest.FreeSpec

import scala.language.higherKinds

import scalaz.Foldable
import scalaz.std.{ListInstances, VectorInstances}

class ToMoreFoldableOpsTest extends FreeSpec with AuxSpecs
    with MoreSeqInstances with ToMoreFoldableOps with MoreTraversableInstances with ListInstances
    with MoreIterableInstances with MoreSetInstances with VectorInstances {
  "doForEach acts on left indices first" in {
    var s = ""
    Seq(1, 2, 3).doForEach(s += _)
    s shouldReturn "123"
  }
  "printPerLine should not overflow" - {
    def test[F[_] : Foldable](f: Int => F[Int]): Unit = {
      Console.withOut(_ => ()) {f(10000).printPerLine()}
    }
    "Seq" in test(Seq.fill(_)(1))
    "List" in test(List.fill(_)(1))
    "Set" in test(1.to(_).toSet)
    "Traversable" in test(Traversable.fill(_)(1))
    "Iterable" in test(Iterable.fill(_)(1))
    "Vector" in test(Vector.fill(_)(1))
  }
  "mapHeadOrElse" - {
    "when Some" in {
      Seq(1).mapHeadOrElse(_ + 1, ???) shouldReturn 2
    }
    "when None" in {
      Nil.mapHeadOrElse((_: Int) => ???, 2) shouldReturn 2
    }
  }
  "head" in {Seq(1, 2, 3).head shouldReturn 1}
  "headOpt" - {
    "empty" in {Seq().headOpt shouldReturn None}
    "nonEmpty" in {Seq(1, 2, 3).headOpt shouldReturn Some(1)}
  }

  "topK" - {
    "empty" in {
      List[String]().topK(10) shouldReturn Nil
    }
    "smaller than requested k should return reverse ordered" in {
      Vector("foo", "bar", "moo").topK(10) shouldReturn Vector("moo", "foo", "bar")
    }
    "actual test" in {
      Vector("foo", "moo", "bar").topK(2) shouldReturn Vector("moo", "foo")
    }
  }
  "bottomK" in {
    Vector("foo", "moo", "bar").bottomK(2) shouldReturn Vector("bar", "foo")
  }
}
