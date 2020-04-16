package common.rich.func

import org.scalatest.AsyncFreeSpec

import scala.concurrent.Future

import common.rich.func.BetterFutureInstances._

import common.test.AsyncAuxSpecs

class RichStreamTTest extends AsyncFreeSpec with AsyncAuxSpecs {
  "iterateM" - {
    "basic" in {
      RichStreamT.iterateM(1)(e => RichOptionT.when(e < 5)(Future successful e + 1))
          .toStream
          .map(_ shouldReturn 1.to(5).toStream)
    }
    "infinite" in {
      RichStreamT.iterateM(1)(e => RichOptionT.pointSome[Future].apply(e + 1)).take(5)
          .toStream
          .map(_ shouldReturn 1.to(5).toStream)
    }
  }
}
