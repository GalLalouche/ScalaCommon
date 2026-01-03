package common.rich.func.scalazz

import org.scalatest.freespec.AsyncFreeSpec
import rx.lang.scala.Observable

import common.rich.func.scalazz.BetterFutureInstances._
import common.rich.func.scalazz.MoreObservableInstances._
import common.rich.func.scalazz.ToMoreFunctorOps._
import scalaz.std.vector.vectorInstance
import scalaz.syntax.bind.ToBindOpsUnapply

import common.rx.RichObservableSpecVer.richObservableSpecVer
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
  "unzip" in {
    val (o1, o2): (Observable[Int], Observable[Int]) = Observable.just(1 -> 2, 3 -> 4).unzip
    o1.toFuture[Vector].shouldEventuallyReturn(Vector(1, 3)) >>
      o2.toFuture[Vector].shouldEventuallyReturn(Vector(2, 4))
  }
  "ifNone" in {
    Vector(None, Some(3), None).ifNone(42) shouldReturn Vector(42, 3, 42)
  }
  "when" in {
    Vector(true, false, true).when(whenTrue = 1, whenFalse = 2) shouldReturn Vector(1, 2, 1)
  }
  "negated" in {
    Vector(true, false, true).negated shouldReturn Vector(false, true, false)
  }
}
