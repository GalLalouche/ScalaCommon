package common.rich.func.kats

import cats.Monad
import cats.data.Writer
import cats.data.Writer.tell
import cats.instances.string.catsKernelStdMonoidForString
import org.scalatest.freespec.AnyFreeSpec

import common.rich.func.kats.ToMoreMonadOps.toMonadBooleanOps

import common.test.AuxSpecs

class ToMoreMonadOpsTest extends AnyFreeSpec with AuxSpecs {
  "conditionals" - {
    type StringWriter[A] = Writer[String, A]
    val writer: Writer[String, Unit] = tell("foo")
    def wrap(b: Boolean) = Monad[StringWriter].point(b)
    "ifTrue" - {
      "true" in { wrap(true).ifTrue(writer).run._1 shouldReturn "foo" }
      "false" in { wrap(false).ifTrue(???).run._1 shouldReturn "" }
    }
    "ifFalse" - {
      "true" in { wrap(true).ifFalse(???).run._1 shouldReturn "" }
      "false" in { wrap(false).ifFalse(writer).run._1 shouldReturn "foo" }
    }
  }
}
