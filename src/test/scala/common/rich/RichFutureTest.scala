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
  val failure: Future[Int] = Future failed new Exception("Derp")
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
        val exception = new IndexOutOfBoundsException("Foobar")
        val f = Future[Int]{throw exception}
        val failure = f.getFailure
        failure.getClass.asInstanceOf[Class[Any]] shouldReturn exception.getClass.asInstanceOf[Class[Any]]
        failure.getMessage shouldReturn exception.getMessage
      }
    }
    "filterWith" - {
      "success" in {
        success.filterWith(_ > 0, "Doesn't matter").get shouldReturn 1
      }
      "failure" in {
        val f = success.filterWith(_ < 0, "Failure message")
        shouldFail(f)
        f.value.get.failed.get.getMessage shouldReturn "Failure message"
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
        val p: Any => Nothing = e => ???
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
