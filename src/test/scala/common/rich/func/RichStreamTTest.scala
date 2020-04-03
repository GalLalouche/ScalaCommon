package common.rich.func

import org.scalatest.AsyncFreeSpec

import scala.concurrent.Future

import scalaz.OptionT
import scalaz.std.scalaFuture.futureInstance

import common.test.AsyncAuxSpecs

class RichStreamTTest extends AsyncFreeSpec with AsyncAuxSpecs {
  "iterateM" - {
    "basic" in {
      RichStreamT.iterateM[Int, Future](1)(
        e => if (e < 5) OptionT(Future.successful(Option(e + 1))) else OptionT.none
      ).toStream
          .map(_ shouldReturn 1.to(5).toStream)
    }
    "infinite" in {
      RichStreamT.iterateM[Int, Future](1)(
        e => OptionT(Future.successful(Option(e + 1)))
      ).take(5)
          .toStream
          .map(_ shouldReturn 1.to(5).toStream)
    }
  }
}
