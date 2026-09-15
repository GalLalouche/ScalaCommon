package common.rich.func.kats

import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.Future

import common.rich.RichFuture.richFutureBlocking
import common.test.AuxSpecs

class PureErrorTest extends AnyFreeSpec with AuxSpecs {
  "wrapTry" - {
    "success" in {
      PureError.tryWrap(Future.successful(42)).get shouldReturn 42
    }
    "failure" in {
      val e = PureError.tryWrap(Future.failed(new Exception("error"))).getFailure
      e.getMessage shouldReturn "error"
    }
    "exception thrown during computation" in {
      val e = PureError
        .tryWrap[Future, String] {
          throw new Exception("error")
        }
        .getFailure
      e.getMessage shouldReturn "error"
    }
  }
}
