package common

import org.scalatest.freespec.AnyFreeSpec

import common.Orderings.{richOrdering, NoneOrder}
import common.test.AuxSpecs

class OrderingsTest extends AnyFreeSpec with AuxSpecs {
  "fromSeqCheck" - {
    def seq = Vector("foo", "moo", "bar")
    "Orders by sequence" in {
      Vector("foo", "bar", "foo")
        .sorted(Orderings.fromSeqCheck(seq)) shouldReturn Vector("foo", "foo", "bar")
    }
    "Throw on element not found" in {
      a[NoSuchElementException] shouldBe thrownBy {
        Vector("foo", "bazz").sorted(Orderings.fromSeqCheck(seq))
      }
    }
  }
  "fromSeqPartial" - {
    def seq = Vector("foo", "moo", "bar")
    "None first" in {
      Vector("foo", "bar", "foo", "bazz")
        .sorted(Orderings.fromSeqPartial(seq, NoneOrder.First))
        .shouldReturn(Vector("bazz", "foo", "foo", "bar"))
    }
    "None last" in {
      Vector("foo", "bar", "foo", "bazz")
        .sorted(Orderings.fromSeqPartial(seq, NoneOrder.Last))
        .shouldReturn(Vector("foo", "foo", "bar", "bazz"))
    }
  }

  "richOrdering" - {
    def ordering = implicitly[Ordering[Int]]
    "noneFirst" in {
      Vector(Some(2), Some(1), None, Some(3))
        .sorted(ordering.noneFirst) shouldReturn Vector(None, Some(1), Some(2), Some(3))
    }
    "noneLast" in {
      Vector(Some(2), Some(1), None, Some(3))
        .sorted(ordering.noneLast) shouldReturn Vector(Some(1), Some(2), Some(3), None)
    }
  }
}
