package common.rich.collections

import org.scalatest.AsyncFreeSpec

import scala.concurrent.Future

import scalaz.std.scalaFuture.futureInstance
import scalaz.Id.Id

import common.rich.RichT._
import common.test.AsyncAuxSpecs

class LazyIterableTest extends AsyncFreeSpec with AsyncAuxSpecs {
  "iterateOptionally" in {
    LazyIterable.iterateOptionally(1)(_.opt.map(_ + 1).filter(_ <= 10))
        .toVector shouldReturn 1.to(10).toVector
  }
  "iterateOptionallyM" - {
    "Id" in {
      LazyIterable.iterateOptionallyM[Int, Id](1)(_.opt.map(_ + 1).filter(_ <= 10))
          .toVector shouldReturn 1.to(10).toVector
    }
    "Future" in {
      LazyIterable.iterateOptionallyM[Int, Future](1)(Future successful _.opt.map(_ + 1).filter(_ <= 10))
          .map(_.toVector shouldReturn 1.to(10).toVector)
    }
  }
}
