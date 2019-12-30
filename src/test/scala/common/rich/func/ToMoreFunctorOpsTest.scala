package common.rich.func

import org.scalatest.AsyncFreeSpec
import rx.lang.scala.Observable

import scalaz.std.scalaFuture.futureInstance
import scalaz.std.vector.vectorInstance
import scalaz.syntax.bind.ToBindOpsUnapply
import common.rich.func.MoreObservableInstances._
import common.rich.func.ToMoreFunctorOps._

import common.rich.RichObservable._
import common.test.AuxSpecs

class ToMoreFunctorOpsTest extends AsyncFreeSpec with AuxSpecs {
  "listen" in {
    var sum = 0
    val $: Observable[Int] = Observable.just(1, 2, 3, 4).listen(sum += _)
    var product = 1
    $.foreach(product *= _)
    sum shouldReturn 10
    product shouldReturn 24
  }
  "unzip" in {
    val (o1, o2): (Observable[Int], Observable[Int]) = Observable.just(1 -> 2, 3 -> 4).unzip
    o1.toFuture[Vector].map(_ shouldReturn Vector(1, 3)) >>
        o2.toFuture[Vector].map(_ shouldReturn Vector(2, 4))
  }
  "ifNone" in {
    Vector(None, Some(3), None).ifNone(42) shouldReturn Vector(42, 3, 42)
  }
  "when" in {
    Vector(true, false, true).when(whenTrue = 1, whenFalse = 2) shouldReturn Vector(1, 2, 1)
  }
}
