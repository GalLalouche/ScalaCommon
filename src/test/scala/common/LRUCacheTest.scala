package common

import org.scalatest.FlatSpec

class LRUCacheTest extends FlatSpec with AuxSpecs {
  "C'tor" should "throw except for negative maxSize" in {
    evaluating(new LRUCache(-1)) should produce[IllegalArgumentException]
  }

  it should "throw except for 0 maxSize" in {
    evaluating(new LRUCache(0)) should produce[IllegalArgumentException]
  }
  "size" should "should remain 1 at most when 1, 1 time" in {
    val $ = new LRUCache[Int, Int](1)
    $(1) = 1
    $.size should be <= 1
    $(2) = 1
    $.size should be <= 1
  }
  it should "should remain 1 at most when 1, 1 value" in {
    val $ = new LRUCache[Int, Int](1)
    $(1) = 1
    $.size should be <= 1
    $(1) = 1
    $.size should be <= 1
    $(1) = 1
    $.size should be <= 1
  }
  it should "should remain 1 at most when 1, 2 time" in {
    val $ = new LRUCache[Int, Int](1)
    $(1) = 1
    $.size should be <= 1
    $(2) = 1
    $.size should be <= 1
  }
  "size" should "should remain 1 at most when 1, loop" in {
    val $ = new LRUCache[Int, Int](1)
    for (i <- 0 to 100000) {
      $(i) = i
      if ($.size > 1)
        throw new AssertionError(s"expected size failed on $i")
    }
  }

  "get" should "return saved value" in {
    val $ = new LRUCache[Int, Int](5)
    for (i <- 0 to 4)
      $(i) = 2 * i
    for (i <- 0 to 4)
      $(i) should be === 2 * i
  }
  it should "should remain 2 at most when 2" in {
    val $ = new LRUCache[Int, Int](2)
    for (i <- 0 to 100000) {
      $(i) = i
      $.size should be <= 2
    }
  }

  "contain" should "refresh the key" in {
    val $ = new LRUCache[Int, Int](2)
    $(1) = 1
    $(2) = 1
    $ contains 1
    $(3) = 1
    $.contains(1) should be === true
    $.contains(2) should be === false
  }

  "update" should "remove old key" in {
    val $ = new LRUCache[Int, Int](2)
    $(1) = 2
    $(1) = 3
    $.size should be === 1
    $(1) should be === 3
  }

  it should "remove the last used key" in {
    val $ = new LRUCache[Int, Int](2)
    $(1) = 1
    $(2) = 2
    $(3) = 3
    $.contains(1) should be === false
    $.contains(2) should be === true
    $.contains(3) should be === true
  }

  it should "not memory leak" in {
    val $ = new LRUCache[Int, String](2)
    for (i <- 0 to 100000) {
      if ($.size > 3)
        throw new AssertionError(s"failed on $i, size was ${$.size}")
      $(i) = "1q2934102937123j1l2;3jalshdz,xmcvzxkcvjfhq09w327410	234uh1nc,zxnczo0x87dfq12l4n1.23eka[s8udawhn4,.1234n12o847ehn"
    }
  }
}