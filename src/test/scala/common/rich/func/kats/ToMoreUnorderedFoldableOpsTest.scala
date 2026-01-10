package common.rich.func.kats

import alleycats.std.all.alleycatsStdIterableTraverse
import cats.{Order, UnorderedFoldable}
import cats.data.State
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.kats.IteratorInstances.iteratorInstances
import common.rich.func.kats.PlainSeqInstances.plainSeqInstances
import common.rich.func.kats.RichState.richState
import common.rich.func.kats.ToMoreUnorderedFoldableOps.toMoreUnorderedFoldableOps

import common.test.AuxSpecs

class ToMoreUnorderedFoldableOpsTest extends AnyFreeSpec with AuxSpecs {
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

  private val emptyIterator: Iterator[Int] = Iterator.empty
  "minimumOption" - {
    "empty" in { emptyIterator.minimumOption shouldBe empty }
    "non-empty" in { Iterator(3, 1, 2).minimumOption.value shouldReturn 1 }
  }

  "maximumOption" - {
    "empty" in { emptyIterator.maximumOption shouldBe empty }
    "non-empty" in { Iterator(3, 1, 2).maximumOption.value shouldReturn 3 }
  }

  "minimumByOption" - {
    "empty" in { emptyIterator.minimumByOption(Math.abs) shouldBe empty }
    "non-empty" in { Iterator(-3, 1, 2).minimumByOption(Math.abs).value shouldReturn 1 }
  }

  "maximumByOption" - {
    "empty" in { emptyIterator.maximumOption shouldBe empty }
    "non-empty" in { Iterator(-3, 1, 2).maximumByOption(Math.abs).value shouldReturn -3 }
  }

  "minimumList" - {
    "empty" in { emptyIterator.minimumList shouldBe empty }
    "non-empty" in { Iterator(-3, 1, -3, 2, 3, 5).minimumList shouldReturn List(-3, -3) }
    "non-trivial-monoid" in {
      Iterator(-3, 1, -3, 2, -1, 3, 5).minimumList(Order.by(_.abs)) shouldReturn List(1, -1)
    }
  }

  "maximumList" - {
    "empty" in { emptyIterator.maximumList shouldBe empty }
    "non-empty" in { Iterator(-3, 1, -3, 2, 5, 3).maximumList shouldReturn List(5) }
    "non-trivial-monoid" in {
      Iterator(-3, 1, -3, 2, -1, 3).maximumList(Order.by(_.abs)) shouldReturn List(-3, -3, 3)
    }
  }

  "unorderedTraverse_" in {
    Iterator(1, 2, 3).unorderedTraverse_(i => State.modify[Int](_ + i)).exec(0) shouldReturn 6
  }
}
