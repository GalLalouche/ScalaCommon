package common.rich.func

import org.scalatest.FreeSpec

import scala.language.higherKinds

import scalaz.Foldable
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.std.stream.streamInstance
import scalaz.std.string.stringInstance
import scalaz.std.vector.vectorInstance
import common.rich.func.MoreIterableInstances._
import common.rich.func.MoreSeqInstances._
import common.rich.func.MoreSetInstances._
import common.rich.func.MoreTraversableInstances._
import common.rich.func.ToMoreFoldableOps._

import common.test.AuxSpecs

class ToMoreFoldableOpsTest extends FreeSpec with AuxSpecs {
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
    "infinite" in {Stream.iterate(1)(_ + 1).headOpt shouldReturn Some(1)}
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
  "asum" in {
    Vector(Option("foo"), Option("moo"), Option("bar")).asum.get shouldReturn "foomoobar"
  }
}
