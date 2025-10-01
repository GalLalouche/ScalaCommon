package common.test.scalazz

import org.scalatest.{Assertion, AsyncTestSuite}

import scala.concurrent.Future

import common.rich.func.scalazz.BetterFutureInstances.betterFutureInstances
import scalaz.StreamT

import common.test.AsyncAuxSpecs

trait StreamTSpecs extends AsyncAuxSpecs {
  self: AsyncTestSuite =>
  implicit class richStreamTFutureSpecs[A]($ : StreamT[Future, A]) {
    def mapValue(f: Stream[A] => Assertion): Future[Assertion] = $.toStream.map(f.apply)
    def valueShouldEventuallyReturn(s: Stream[A]): Future[Assertion] = mapValue(_ shouldReturn s)
  }
}
