package common.rich.func.kats

import java.net.ConnectException
import java.util.concurrent.{Executors, Semaphore}

import cats.implicits.catsSyntaxApplicativeError
import org.scalatest.EitherValues._
import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

import common.rich.func.kats.ToMoreMonadErrorOps._

import common.rich.RichFuture.richFutureBlocking
import common.rich.RichT.lazyT
import common.test.AuxSpecs

class ToMoreMonadErrorOpsTest extends AnyFreeSpec with AuxSpecs {
  private lazy val unusedError: String = ???
  private def is(expected: Int)(actual: Int) =
    if (actual == expected) None else Some(s"Is not $expected")
  "toMoreMonadErrorOps" - {
    val success: BoxOrMsg[Int] = Box(42)
    val failure: BoxOrMsg[Int] = Msg("failure")
    "collectHandle" - {
      "success" in {
        success.collectHandle { case _ => ??? }.get shouldReturn 42
      }
      "failure" - {
        "partial doesn't handle" in {
          failure.collectHandle { case "foobar" =>
            54
          }.getFailure shouldReturn "failure"
        }
        "partial handles" in {
          failure.collectHandle { case "failure" =>
            54
          }.get shouldReturn 54
        }
        "Exception" in {
          val e: ContainerOrError[Int] = Error(new ConnectException("Failed to connect"))
          e.collectHandle { case _: ConnectException =>
            42
          }.get shouldReturn 42
        }
      }
    }
    "orElse" - {
      "success" in {
        success.orElse(???).get shouldReturn 42
      }
      "failure" in {
        failure.orElseFlat(43).get shouldReturn 43
      }
    }
    "handleErrorButKeepOriginal" - {
      "success" in {
        success.handleButKeepOriginal(_ => ???).get shouldReturn 42
      }
      "failure" - {
        "to success" in {
          failure.handleButKeepOriginal(success.const).get shouldReturn 42
        }
        "to failure" in {
          failure.handleButKeepOriginal(Msg("failure 2").const).getFailure shouldReturn "failure"
        }
      }
    }
    "filterWith" - {
      "success" - {
        "pred is true" in {
          success.filterWith(_ == 42, unusedError).get shouldReturn 42
        }
        "pred is false" in {
          success
            .filterWith(_ == 43, "expected 43 but was 42")
            .getFailure shouldReturn "expected 43 but was 42"
        }
      }
      "failure" in {
        failure.filterWith(_ => ???, ???).getFailure shouldReturn "failure"
      }
    }
    "filterMap" - {
      "success" - {
        "pred is true" in {
          success.filterMap(is(42)).get shouldReturn 42
        }
        "pred is false" in {
          success.filterMap(is(43)).getFailure shouldReturn "Is not 43"
        }
      }
      "failure" in {
        failure.filterMap(_ => ???).getFailure shouldReturn "failure"
      }
    }
    "mFilter" - {
      "success" - {
        "pred is true" in {
          success.mFilter(e => Box(e == 42), _ => ???).get shouldReturn 42
        }
        "pred is false" in {
          success
            .mFilter(e => Box(e == 41), e => s"expected 41 but was $e")
            .getFailure shouldReturn "expected 41 but was 42"
        }
      }
      "failure" in {
        failure.mFilter(_ => ???, _ => ???).getFailure shouldReturn "failure"
      }
    }
    "mapError" - {
      "success" in {
        success.mapError(_ => ???).get shouldReturn 42
      }
      "failure" in {
        failure.mapError(_.toUpperCase).getFailure shouldReturn "FAILURE"
      }
    }
    "listenError" - {
      "success" in {
        success.listenError(_ => ???).get
      }
      "failure" in {
        var x = 10
        failure.listenError(x += _.length).getFailure
        x shouldReturn 17
      }
    }
    "guard" - {
      "true returns a unit" in {
        ToMoreMonadErrorOps.guard[BoxOrMsg, String](b = true, ???) shouldReturn Box(())
      }
      "false returns an error" in {
        ToMoreMonadErrorOps.guard[BoxOrMsg, String](b = false, "Failure") shouldReturn Msg(
          "Failure",
        )
      }
    }
    "listenEither" - {
      "success" in {
        val sb = new StringBuilder
        success.listenEither(sb append _.value).get shouldReturn 42
        sb.toString shouldReturn "42"
      }
      "failure" in {
        val sb = new StringBuilder
        failure.listenEither(sb append _.left.value).getFailure shouldReturn "failure"
        sb.toString shouldReturn "failure"
      }
    }
    "listenAny" - {
      implicit val ec: ExecutionContextExecutor =
        ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
      val semaphore = new Semaphore(0)
      // Checks for by-name
      "success" in {
        val sb = new StringBuilder
        val future = Future {
          semaphore.acquire();
          42
        }.listenAny(sb.append("test"))
        sb.toString shouldBe empty
        semaphore.release()
        future.get
        sb.toString shouldReturn "test"
      }
      "failure" in {
        val sb = new StringBuilder
        val future = Future {
          semaphore.acquire()
          throw new Exception("Whoops")
        }.listenAny(sb.append("test"))
        sb.toString shouldBe empty
        semaphore.release()
        future.getFailure.getMessage shouldReturn "Whoops"
        sb.toString shouldReturn "test"
      }
    }
  }

  "toMoreMonadErrorThrowableOps" - {
    val success: ContainerOrError[Int] = Container(42)
    val failure: ContainerOrError[Int] = Error(new RuntimeException("failure"))
    "filterEquals" - {
      "success" - {
        "pred is true" in {
          success.filterEquals(42).get shouldReturn 42
        }
        "pred is false" in {
          success.filterEquals(43).getFailure.getMessage shouldReturn
            "Expected: <43>,\n" +
            "but was:  <42>"
        }
      }
      "failure" in {
        val ex = failure.filterEquals(???).getFailure
        ex.getMessage shouldReturn "failure"
        ex shouldBe a[RuntimeException]
      }
    }
    "filterWithMessage" - {
      "success" - {
        "pred is true" in {
          success.filterWithMessageF(_ % 42 == 0, _ => ???).get shouldReturn 42
        }
        "pred is false" in {
          success
            .filterWithMessage(_ % 42 == 1, "foobar")
            .getFailure
            .getMessage shouldReturn "foobar"
        }
      }
      "failure" in {
        val ex = failure.filterWithMessageF(_ => ???, _ => ???).getFailure
        ex.getMessage shouldReturn "failure"
        ex shouldBe a[RuntimeException]
      }
    }
    "filterMap" - {
      "success" - {
        "pred is true" in {
          success.filterMapMessage(is(42)).get shouldReturn 42
        }
        "pred is false" in {
          success.filterMapMessage(is(43)).getFailure.getMessage shouldReturn "Is not 43"
        }
      }
      "failure" in {
        val ex = failure.filterMapMessage(_ => ???).getFailure
        ex.getMessage shouldReturn "failure"
        ex shouldBe a[RuntimeException]
      }
    }
    "filterWithStackTrace" - {
      "success" - {
        "pred is true" in {
          success.filterWithMessageF(_ % 42 == 0, _ => ???).get shouldReturn 42
        }
        "pred is false" in {
          def foo: ContainerOrError[Any] = {
            def bar = {
              def baz = {
                def qux =
                  success.filterWithStacktrace(_ < 0, "Foobar")
                qux
              }
              baz
            }
            bar
          }

          val ex = foo.getFailure
          ex.getMessage shouldReturn "Foobar"
          ex.getStackTrace
            .zip(Vector("qux", "baz", "bar", "foo"))
            .map(e => e._1.toString contains e._2)
            .forall(identity) shouldReturn true
        }
      }
      "failure" in {
        val ex = failure.filterWithStacktraceF(_ => ???, _ => ???).getFailure
        ex.getMessage shouldReturn "failure"
        ex shouldBe a[RuntimeException]
      }
    }

    "guard" - {
      "true returns a unit" in {
        ToMoreMonadErrorOps.guardMessage[ContainerOrError](b = true, ???) shouldReturn Container(())
      }
      "false returns an error" in {
        ToMoreMonadErrorOps
          .guardMessage[ContainerOrError](b = false, "Failure")
          .getFailure
          .getMessage shouldReturn "Failure"
      }
    }
  }
}
