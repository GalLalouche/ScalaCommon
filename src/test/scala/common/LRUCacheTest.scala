package common

import org.scalatest.FreeSpec

import common.test.AuxSpecs

class LRUCacheTest extends FreeSpec with AuxSpecs {
  "C'tor" - {
    "throw except for negative maxSize" in {
      an[IllegalArgumentException] should be thrownBy new LRUCache(-1)
    }
    "throws except for 0 maxSize" in {
      an[IllegalArgumentException] should be thrownBy new LRUCache(0)
    }
  }

  "size" - {
    "remains 1 at most when 1, 1 time" in {
      val $ = new LRUCache[Int, Int](1)
      $(1) = 1
      $.size should be <= 1
      $(2) = 1
      $.size should be <= 1
    }
    "remains 1 at most when 1, 1 value" in {
      val $ = new LRUCache[Int, Int](1)
      $(1) = 1
      $.size should be <= 1
      $(1) = 1
      $.size should be <= 1
      $(1) = 1
      $.size should be <= 1
    }
    "remains 1 at most when 1, 2 time" in {
      val $ = new LRUCache[Int, Int](1)
      $(1) = 1
      $.size should be <= 1
      $(2) = 1
      $.size should be <= 1
    }
  }
  "size remains 1 at most when 1, loop" in {
    val $ = new LRUCache[Int, Int](1)
    for (i <- 0 to 100000) {
      $(i) = i
      if ($.size > 1)
        throw new AssertionError(s"expected size failed on $i")
    }
  }

  "get" - {
    "returns saved value" in {
      val $ = new LRUCache[Int, Int](5)
      for (i <- 0 to 4)
        $(i) = 2 * i
      for (i <- 0 to 4)
        $(i) shouldReturn 2 * i
    }
    "remains 2 at most when 2" in {
      val $ = new LRUCache[Int, Int](2)
      for (i <- 0 to 100000) {
        $(i) = i
        $.size should be <= 2
      }
    }
  }

  "contain refreshes the key" in {
    val $ = new LRUCache[Int, Int](2)
    $(1) = 1
    $(2) = 1
    $ contains 1
    $(3) = 1
    $.contains(1) shouldReturn true
    $.contains(2) shouldReturn false
  }

  "update" - {
    "removes old key" in {
      val $ = new LRUCache[Int, Int](2)
      $(1) = 2
      $(1) = 3
      $.size shouldReturn 1
      $(1) shouldReturn 3
    }

    "removes the last used key" in {
      val $ = new LRUCache[Int, Int](2)
      $(1) = 1
      $(2) = 2
      $(3) = 3
      $.contains(1) shouldReturn false
      $.contains(2) shouldReturn true
      $.contains(3) shouldReturn true
    }

    "doesn't memory leak" in {
      val $ = new LRUCache[Int, String](2)
      for (i <- 0 to 100000) {
        if ($.size > 3)
          throw new AssertionError(s"failed on $i, size was ${$.size}")
        $(i) = "1q2934102937123j1l2;3jalshdz,xmcvzxkcvjfhq09w327410 234uh1nc,zxnczo0x87dfq12l4n1.23eka[s8udawhn4,.1234n12o847ehn"
      }
    }
  }
}
