package common.rich.collections

import common.AuxSpecs
import common.rich.collections.RichSet._
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.anyvals.PosZInt._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable

class RichSetTest extends PropSpec with AuxSpecs with PropertyChecks {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 1000)
  lazy val genSet: Gen[Set[Int]] =
    for (i <- Gen.choose(10, 1000)) yield {
      val $ = new mutable.HashSet[Int]()
      val g = Gen.choose(Int.MinValue, Int.MaxValue)
      while ($.size < i)
        $ += g.sample.get
      $.toSet
    }
  implicit lazy val arbSet: Arbitrary[Set[Int]] = Arbitrary(genSet)
  property("The empty set is <= for all sets") {
    forAll { xs: Set[Int] => {
      Set.empty <= xs shouldReturn true
    }
    }
  }

  property("All sets are <= than themselves") {
    forAll { $: Set[Int] =>
      $ <= $ shouldReturn true
    }
  }

  property("< is both <= and =") {
    forAll { xs: Set[Int] => {
      xs.tail < xs shouldReturn true
      xs < xs shouldReturn false
      xs < xs.tail shouldReturn false
    }
    }
  }

  property("> and >=") {
    forAll { xs: Set[Int] => {
      xs > xs.tail shouldReturn true
      xs > xs shouldReturn false
      xs.tail > xs shouldReturn false
      xs >= xs shouldReturn true
      xs >= xs.tail shouldReturn true
    }
    }
  }

  property("\\ should be of max size itself") {
    forAll { (xs: Set[Int], ys: Set[Int]) =>
      (xs \ ys).size should be <= xs.size
    }
  }
  property("\\ should only contain members of xs") {
    forAll { (xs: Set[Int], ys: Set[Int]) =>
      (xs \ ys) <= xs shouldReturn true
    }
  }
  property("\\ intersection with ys should be empty") {
    forAll { (xs: Set[Int], ys: Set[Int]) =>
      (xs \ ys).intersect(ys).isEmpty shouldReturn true
    }
  }

  property("The empty set is disjoint to all sets") {
    forAll { $: Set[Int] =>
      Set[Int]() isDisjointTo $ shouldReturn true
    }
  }
  property("No set is disjoint to itself but the empty set") {
    forAll { $: Set[Int] =>
      $ isDisjointTo $ shouldReturn $.isEmpty
    }
  }
  property("\\ should be disjoint to ys") {
    forAll { (xs: Set[Int], ys: Set[Int]) =>
      (xs \ ys) isDisjointTo ys shouldReturn true
    }
  }
}
