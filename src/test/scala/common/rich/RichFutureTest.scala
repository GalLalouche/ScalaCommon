package common.rich

import common.AuxSpecs
import common.rich.RichFuture._
import org.scalatest.FreeSpec

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

class RichFutureTest extends FreeSpec with AuxSpecs {
  val success: Future[Int] = Future successful 1
  val exception = new Exception("Derp")
  val failure: Future[Int] = Future failed exception
  def shouldFail(f: Future[Any]) = {
    Await.ready(f, Duration.Inf)
    val t = f.value.get
    t.isFailure shouldReturn true
  }
  "RichFuture" - {
    "get" - {
      "success" in {
        val f = Future {
          Thread.sleep(10)
          5
        }
        f.get shouldReturn 5
      }
      "failure" in {
        val f = Future {
          Thread.sleep(10)
          throw new Exception()
        }
        an[Exception] should be thrownBy f.get
      }
    }
    "get exception" - {
      "when success should throw" in {
        val f = Future[Int](1)
        an[UnsupportedOperationException] should be thrownBy f.getFailure
      }
      "when failure should return the error" in {
        val f = failure
        val e = f.getFailure
        e shouldReturn exception
      }
    }
    "filterWith" - {
      "success" in {
        success.filterWith(_ > 0, "Doesn't matter").get shouldReturn 1
      }
      "failure" in {
        val f = success.filterWith(_ < 0, "Failure message")
        shouldFail(f)
        val ex = f.failed.get
        ex.getMessage shouldReturn "Failure message"
        ex shouldBe an[RichFuture.FilteredException]
      }
      "stacktrace" in {
        def foo: Future[Any] = {
          def bar = {
            def baz = {
              def qux = {
                Future.apply(false).filterWithStacktrace(identity, "Foobar")
              }
              qux
            }
            baz
          }
          bar
        }
        val f = foo
        val stackTrace = Try(f.get).failed.get.getStackTrace
        stackTrace zip List("qux", "baz", "bar", "foo") map (e => e._1.toString contains e._2) forall identity shouldReturn true
      }
    }
    "orElse" - {
      "success" in {
        success.orElse(???).get shouldReturn 1
      }
      "failure" in {
        failure.orElse(10).get shouldReturn 10
      }
    }
    "orElseTry" - {
      "success" in {
        success.orElseTry(???).get shouldReturn 1
      }
      "failure" - {
        "to success" in {
          failure.orElseTry(Future successful 10).get shouldReturn 10
        }
        "to failure" in {
          shouldFail(failure.orElseTry(failure))
        }
      }
    }
    "consume" - {
      "success" in {
        val list = new ListBuffer[Int]
        val value = success.consume(list.+=).get
        list shouldReturn ListBuffer(value)
      }
    }
    "consumeTry" - {
      "success" in {
        var succeeded = false
        val value = success.consumeTry(e => succeeded = e.isSuccess).get
        value shouldReturn 1
        succeeded shouldReturn true
      }
      "failure" in {
        var succeeded = true
        val value = failure.consumeTry(e => succeeded = e.isSuccess).getFailure
        value shouldReturn exception
        succeeded shouldReturn false
      }
    }
  }
  "RichOptionFuture" - {
    val success: Future[Option[Int]] = Future successful Some(1)
    val failure: Future[Option[Int]] = Future failed new Exception("Derp")
    val none: Future[Option[Int]] = Future successful None
    "ifNone" - {
      "success" - {
        "some" in {
          success.ifNone(???).get shouldReturn 1
        }
        "none" in {
          none.ifNone(10).get shouldReturn 10
        }
      }
      "failure" in {
        shouldFail(failure.ifNone(10))
      }
    }
    "ifNoneTry" - {
      "success" - {
        "some" in {
          success.ifNoneTry(???).get shouldReturn 1
        }
        "none" in {
          none.ifNoneTry(Future successful 10).get shouldReturn 10
        }
      }
      "failure" in {
        shouldFail(failure.ifNoneTry(Future successful 10))
      }
    }
    "filterFuture" - {
      "some" - {
        "true" in {
          success.filterFuture(Future(_) map (_ > 0)).get.get shouldReturn 1
        }
        "false" in {
          success.filterFuture(Future(_) map (_ < 0)).get shouldReturn None
        }
      }
      "none" - {
        val p: Any => Nothing = _ => ???
        "true" in {
          none.filterFuture(p).get shouldReturn None
        }
        "false" in {
          none.filterFuture(p).get shouldReturn None
        }
      }
    }
  }
}
