package common.uf

import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.freespec.AnyFreeSpec

import common.test.AuxSpecs

class ImmutableUnionMapTest extends AnyFreeSpec with AuxSpecs {
  private val map = ImmutableUnionMap(Map(1 -> 2, 2 -> 3, 3 -> 4)).union(1, 2, 5)
  "Basic test" in {
    val map = ImmutableUnionMap(Map(1 -> 2, 2 -> 3, 3 -> 4)).union(1, 2, 5)
    map(1) shouldReturn 5
    map(2) shouldReturn 5
    map(3) shouldReturn 4
    map.get(4) shouldBe 'empty
  }
  "iterator" in {
    map shouldContainExactly (1 -> 5, 2 -> 5, 3 -> 4)
  }
  "update" - {
    "new key throws" in {
      an[IllegalArgumentException] shouldBe thrownBy(map.update(4 -> 6))
    }
    "existing key (un-unioned)" in {
      map.update(3 -> 6) shouldContainExactly (1 -> 5, 2 -> 5, 3 -> 6)
    }
    "existing key (unioned)" in {
      // checks for both representatives
      map.update(1 -> 6) shouldContainExactly (1 -> 6, 2 -> 6, 3 -> 4)
      map.update(2 -> 6) shouldContainExactly (1 -> 6, 2 -> 6, 3 -> 4)
    }
  }
  "unionUnsafe" - {
    "throws if keys have different values" in {
      an[IllegalArgumentException] shouldBe thrownBy(map.unionUnsafe(1, 3))
    }
    "works if keys have same value" in {
      val map =
        ImmutableUnionMap(Map(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 4)).unionUnsafe(3, 4).update(4 -> 5)
      map shouldContainExactly (1 -> 2, 2 -> 3, 3 -> 5, 4 -> 5)
    }
  }
  "get" - {
    "Returns none if doesn't exist" in {
      map.get(5) shouldReturn None
    }
    "Returns some if exists (un-unioned)" in {
      map.get(3).value shouldReturn 4
    }
    "Returns some if exists (unioned)" in {
      map.get(1).value shouldReturn 5
      map.get(2).value shouldReturn 5
    }
  }
  "mapValues" in {
    val newMap = map.mapValues(_ + 3)
    newMap(1) shouldReturn 8
    newMap(2) shouldReturn 8
    newMap(3) shouldReturn 7
  }
}
