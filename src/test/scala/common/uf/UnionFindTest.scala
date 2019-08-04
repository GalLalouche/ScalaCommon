package common.uf

import common.AuxSpecs
import org.scalatest.FreeSpec

abstract class UnionFindTest extends FreeSpec with AuxSpecs {
  private val vector = Vector("foo", "bar", "bazz", "quux", "moo")
  protected def createUnionFind[A](xs: IterableOnce[A]): UnionFind[A]
  private val $ = createUnionFind(vector)
  private def deepSets[A](intses: Iterable[Iterable[A]]): Set[Set[A]] = intses.map(_.toSet).toSet
  "all elements begin in different sets" in {
    $.sets shouldReturn vector.map(Set(_))
    $.numberOfSets shouldReturn 5
    $.hasSingleSet shouldReturn false
  }

  "union" - {
    "after union, both items are in the same set" in {
      val unioned = $.union("foo", "bar")
      deepSets(unioned.sets) shouldEqual Set(Set("foo", "bar"), Set("bazz"), Set("quux"), Set("moo"))
      unioned.numberOfSets shouldReturn 4
    }
    "deep union" in {
      val vector = 1 to 10
      val $ = createUnionFind(vector).union(1, 2).union(4, 5).union(7, 10).union(4, 7).union(10, 2)
          .union(6, 8).union(6, 9)
          // Pointless joins but they test the numberOfSets
          .union(1, 10).union(5, 7).union(6, 6)
      deepSets($.sets) shouldEqual Set(Set(1, 2, 4, 5, 7, 10), Set(6, 8, 9), Set(3))
      $.numberOfSets shouldReturn 3
      $.hasSingleSet shouldReturn false
    }
  }

  "singleSet" in {
    createUnionFind(Vector(1, 2, 3)).union(1, 2).union(2, 3).hasSingleSet shouldReturn true
  }
}
