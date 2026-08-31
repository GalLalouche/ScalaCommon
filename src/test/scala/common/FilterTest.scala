package common

import org.scalatest.freespec.AnyFreeSpec

import common.test.AuxSpecs

class FilterTest extends AnyFreeSpec with AuxSpecs {
  private val even: Filter[Int] = _ % 2 == 0
  private val positive: Filter[Int] = _ > 0

  "passes" - {
    "returns the predicate result" in {
      even.passes(2) shouldReturn true
      even.passes(3) shouldReturn false
    }
  }

  "fails" in {
    even.fails(2) shouldReturn false
    even.fails(3) shouldReturn true
  }

  "and" in {
    even.and(positive).passes(2) shouldReturn true
    even.and(positive).passes(-2) shouldReturn false
    even.and(positive).passes(3) shouldReturn false
  }

  "or" in {
    even.or(positive).passes(2) shouldReturn true
    even.or(positive).passes(3) shouldReturn true
    even.or(positive).passes(-3) shouldReturn false
  }

  "negate" in {
    even.negate.passes(2) shouldReturn false
    even.negate.passes(3) shouldReturn true
  }

  "always" - {
    "passes every value" in {
      Filter.always.passes("value") shouldReturn true
    }
    "combines as the identity filter" in {
      Filter.always.and(even).passes(3) shouldReturn false
      Filter.always.or(even).passes(3) shouldReturn true
    }
    "negates to never" in {
      Filter.always.negate.passes(()) shouldReturn false
    }
  }

  "never" - {
    "rejects every value" in {
      Filter.never.passes("value") shouldReturn false
    }
    "combines as the absorbing filter" in {
      Filter.never.and(even).passes(2) shouldReturn false
      Filter.never.or(even).passes(2) shouldReturn true
    }
    "negates to always" in {
      Filter.never.negate.passes(()) shouldReturn true
    }
  }
}
