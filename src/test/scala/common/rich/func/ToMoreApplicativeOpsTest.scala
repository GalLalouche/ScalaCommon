package common.rich.func

import common.AuxSpecs
import org.scalatest.FreeSpec
import scalaz.Writer
import scalaz.std.StringInstances

class ToMoreApplicativeOpsTest extends FreeSpec with AuxSpecs with StringInstances with ToMoreApplicativeOps {
  "withFilter" - {
    type StringWriter[A] = Writer[String, A]
    def writer: StringWriter[Unit] = Writer("foo", ())
    def log(u: Unit): StringWriter[Unit] = Writer("bar", ())

    "when true" in {
      val result: StringWriter[Unit] = for {
        w <- writer
        w2 <- log(w) if true
      } yield w2

      result.run._1 shouldReturn "foobar"
    }

    "when false" in {
      val result: StringWriter[Unit] = for {
        w <- writer
        w2 <- log(w) if false
      } yield w2

      result.run._1 shouldReturn "foo"
    }
  }
}