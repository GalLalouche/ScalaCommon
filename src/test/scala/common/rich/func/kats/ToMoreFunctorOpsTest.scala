package common.rich.func.kats

import cats.implicits.{catsSyntaxIfF, catsSyntaxOptionId, none}
import org.scalatest.freespec.AsyncFreeSpec
import rx.lang.scala.Observable

import common.rich.func.kats.ObservableInstances.observableInstances
import common.rich.func.kats.ToMoreFunctorOps.toMoreFunctorOps

import common.test.AsyncAuxSpecs

class ToMoreFunctorOpsTest extends AsyncFreeSpec with AsyncAuxSpecs {
  "listen" in {
    var sum = 0
    val $ : Observable[Int] = Observable.just(1, 2, 3, 4).listen(sum += _)
    var product = 1
    $.foreach(product *= _)
    sum shouldReturn 10
    product shouldReturn 24
  }
  ">|" in {
    none >| ??? shouldReturn None
    42.some >| "foobar" shouldReturn Some("foobar")
  }
  "ifNone" in {
    Vector(None, Some(3), None).ifNone(42) shouldReturn Vector(42, 3, 42)
  }
  "when" in {
    Vector(true, false, true).ifF(ifTrue = 1, ifFalse = 2) shouldReturn Vector(1, 2, 1)
  }
  "negated" in {
    Vector(true, false, true).negated shouldReturn Vector(false, true, false)
  }
}
