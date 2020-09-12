package common.rich.func

import java.util.concurrent.{Executors, ExecutorService}
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.FreeSpec

import scala.concurrent.{ExecutionContext, Future}

import scalaz.std.string.stringInstance
import scalaz.WriterT.tell
import common.rich.func.ToMoreApplicativeOps._

import common.rich.RichFuture.richFuture
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
  "lazy" - {
    "when" - {
      import BetterFutureInstances._
      implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
      def failing: Future[Int] = ???
      def success(ai: AtomicInteger): Future[Int] = {
        Future {
          ai.incrementAndGet()
        }
      }
      "false" in {
        failing.whenMLazy(false).get shouldReturn()
      }
      "true" in {
        val ai = new AtomicInteger(0)
        success(ai).whenMLazy(true).get shouldReturn()
        ai.get shouldReturn 1
      }
    }
  }
}
