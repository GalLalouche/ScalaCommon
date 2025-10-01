package common.rich.func.kats

import alleycats.std.all.alleycatsStdIterableTraverse
import cats.UnorderedFoldable
import org.scalatest.FreeSpec

import common.rich.func.kats.PlainSeqInstances.plainSeqInstances
import common.rich.func.kats.ToMoreUnorderedFoldableOps.toMoreUnorderedFoldableOps

import common.test.AuxSpecs

class ToMoreUnorderedFoldableOpsTest extends FreeSpec with AuxSpecs {
  "printPerLine should not overflow" - {
    def test[F[_]: UnorderedFoldable](f: Int => F[Int]): Unit =
      Console.withOut(_ => ())(f(10000).printPerLine())
    "Seq" in test(Seq.fill(_)(1))
    "List" in test(List.fill(_)(1))
    "Set" in test(1.to(_).toSet)
    "Iterable" in test(Iterable.fill(_)(1))
    "Vector" in test(Vector.fill(_)(1))
  }

  "topK" - {
    "empty" in {
      Vector[String]().topK(10) shouldReturn Nil
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
