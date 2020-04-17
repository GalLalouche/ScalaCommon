package common.rich.func

import org.scalatest.FreeSpec

import scalaz.std.string.stringInstance
import scalaz.WriterT.tell
import common.rich.func.ToMoreApplicativeOps._

import common.test.AuxSpecs

class ToMoreApplicativeOpsTest extends FreeSpec with AuxSpecs {
  "withFilter" - {
    def go(b: Boolean) = for {
      _ <- tell("foo")
      _ <- tell("bar") if b
    } yield ()
    "when true" in {go(true).run._1 shouldReturn "foobar"}
    "when false" in {go(false).run._1 shouldReturn "foo"}
  }
}
