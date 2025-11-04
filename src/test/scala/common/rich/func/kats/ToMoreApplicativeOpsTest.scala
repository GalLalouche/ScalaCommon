package common.rich.func.kats

import cats.data.Writer
import cats.data.Writer.tell
import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.kats.ToMoreApplicativeOps._

import common.test.AuxSpecs

class ToMoreApplicativeOpsTest extends AnyFreeSpec with AuxSpecs {
  "withFilter" - {
    def go(b: Boolean): Writer[String, Unit] = for {
      _ <- tell("foo")
      _ <- tell("bar") if b
    } yield ()
    "when true" in { go(true).run._1 shouldReturn "foobar" }
    "when false" in { go(false).run._1 shouldReturn "foo" }
  }
}
