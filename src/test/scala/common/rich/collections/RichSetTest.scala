package common.rich.collections

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.util.Buildable
import org.scalactic.anyvals.PosZInt._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

import common.rich.collections.RichSet._
import common.test.AuxSpecs

class RichSetTest extends PropSpec with AuxSpecs with PropertyChecks {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 1000)

  private implicit def BuildableSet[T]: Buildable[T, Set[T]] = new Buildable[T, Set[T]] {
    override def builder = Set.newBuilder[T]
  }
  private implicit val genSet: Gen[Set[Int]] = Gen.buildableOf(arbitrary[Int])

  property("Sets can be neither <= nor >=") {
    forAll { set: Set[Int] =>
      whenever(set.size >= 3) {
        val subsetSize = set.size / 2 + 1
        val asList = set.toList
        val xs = asList.take(subsetSize).toSet
        val ys = asList.reverse.take(subsetSize).toSet
        xs <= ys shouldReturn false
        xs < ys shouldReturn false
        xs >= ys shouldReturn false
        xs < ys shouldReturn false
      }
    }
  }
  property("The empty set is <= for all sets") {
    forAll { xs: Set[Int] => {
      Set.empty <= xs shouldReturn true
    }
    }
  }

  property("All sets are <= and >= themselves") {
    forAll { $: Set[Int] =>
      $ <= $ shouldReturn true
      $ >= $ shouldReturn true
    }
  }

  property("< is both <= and =") {
    forAll { xs: Set[Int] =>
      whenever(xs.nonEmpty) {
        xs.tail < xs shouldReturn true
        xs < xs shouldReturn false
        xs < xs.tail shouldReturn false
      }
    }
  }

  property("> and >=") {
    forAll {
      xs: Set[Int] => {
        whenever(xs.nonEmpty) {
          xs > xs.tail shouldReturn true
          xs > xs shouldReturn false
          xs.tail > xs shouldReturn false
          xs >= xs shouldReturn true
          xs >= xs.tail shouldReturn true
        }
      }
    }
  }

  property("The empty set is disjoint to all sets") {
    forAll {
      $: Set[Int] =>
        Set[Int]() isDisjointTo $ shouldReturn true
    }
  }
  property("No set is disjoint to itself but the empty set") {
    forAll {
      $: Set[Int] =>
        $ isDisjointTo $ shouldReturn $.isEmpty
    }
  }
  property("&~ should be disjoint to ys") {
    forAll {
      (xs: Set[Int], ys: Set[Int]) =>
        xs &~ ys isDisjointTo ys shouldReturn true
    }
  }
}
