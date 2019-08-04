package common.rich.func

import org.scalatest.FreeSpec

import scalaz.MonadError
import common.rich.func.MoreMonadErrorSyntax._

import common.AuxSpecs
import common.rich.RichT._

private object MoreMonadErrorSyntaxTest {
  private sealed trait BoxOrMsg[+T] {
    def get: T
    def getFailure: String
  }
  private case class Box[T](get: T) extends BoxOrMsg[T] {
    override def getFailure: String = throw new Exception(s"Box <$this> isn't a failure")
  }
  private case class Msg(getFailure: String) extends BoxOrMsg[Nothing] {
    override def get: Nothing = throw new Exception(s"Error <$this> isn't a box")
  }
  private object BoxOrMsg {
    implicit object MonadErrorImpl extends MonadError[BoxOrMsg, String] {
      override def raiseError[A](e: String): BoxOrMsg[A] = Msg(e)
      override def handleError[A](fa: BoxOrMsg[A])(f: String => BoxOrMsg[A]): BoxOrMsg[A] =
        fa match {
          case b@Box(_) => b
          case Msg(e) => f(e)
        }
      override def bind[A, B](fa: BoxOrMsg[A])(f: A => BoxOrMsg[B]): BoxOrMsg[B] =
        fa match {
          case e@Msg(_) => e
          case Box(e) => f(e)
        }
      override def point[A](a: => A): BoxOrMsg[A] = Box(a)
    }
  }

  private sealed trait ContainerOrError[+T] {
    def get: T
    def getFailure: Throwable
  }
  private case class Container[T](get: T) extends ContainerOrError[T] {
    override def getFailure: Throwable = throw new Exception(s"Box <$this> isn't a failure")
  }
  private case class Error(getFailure: Throwable) extends ContainerOrError[Nothing] {
    override def get: Nothing = throw new Exception(s"Error <$this> isn't a box")
  }
  private object ContainerOrError {
    implicit object MonadErrorImpl extends MonadError[ContainerOrError, Throwable] {
      override def raiseError[A](e: Throwable): ContainerOrError[A] = Error(e)
      override def handleError[A](fa: ContainerOrError[A])(f: Throwable => ContainerOrError[A]) =
        fa match {
          case e@Container(_) => e
          case Error(t) => f(t)
        }
      override def bind[A, B](fa: ContainerOrError[A])(f: A => ContainerOrError[B]) = fa match {
        case e@Error(_) => e
        case Container(e) => f(e)
      }
      override def point[A](a: => A): ContainerOrError[A] = Container(a)
    }
  }
}

class MoreMonadErrorSyntaxTest extends FreeSpec with AuxSpecs {
  import common.rich.func.MoreMonadErrorSyntaxTest._

  private lazy val unusedError: String = ???
  private def is(expected: Int)(actual: Int) =
    if (actual == expected) None else Some(s"Is not $expected")
  "MoreMonadErrorSyntax" - {
    val success: BoxOrMsg[Int] = Box(42)
    val failure: BoxOrMsg[Int] = Msg("failure")
    "handleErrorFlat" - {
      "success" in {
        success.handleErrorFlat(_ => ???).get shouldReturn 42
      }
      "failure" in {
        failure.handleErrorFlat(_.length).get shouldReturn 7
      }
    }
    "orElse" - {
      "success" in {
        success.orElse(???).get shouldReturn 42
      }
      "failure" in {
        failure.orElse(43).get shouldReturn 43
      }
    }
    "orElseTry" - {
      "success" in {
        success.orElseTry(???).get shouldReturn 42
      }
      "failure" - {
        "to success" in {
          failure.orElseTry(success).get shouldReturn 42
        }
        "to failure" in {
          failure.orElseTry(failure).getFailure shouldReturn "failure"
        }
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
          val failure2: BoxOrMsg[Int] = Msg("failure 2")
          failure.handleButKeepOriginal(failure2.const).getFailure shouldReturn "failure"
        }
      }
    }
    "filterWith" - {
      "success" - {
        "pred is true" in {
          success.filterWith(_ == 42, unusedError).get shouldReturn 42
        }
        "pred is false" in {
          success.filterWith(_ == 43, "expected 43 but was 42")
              .getFailure shouldReturn "expected 43 but was 42"
        }
      }
      "failure" in {
        failure.filterWith(_ => ???, {???}).getFailure shouldReturn "failure"
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
          success.mFilter(e => Box(e == 41), e => s"expected 41 but was $e")
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
  }

  "MoreMonadErrorThrowableSyntax" - {
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
          success.filterWithMessage(_ % 42 == 1, "foobar")
              .getFailure.getMessage shouldReturn "foobar"
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
                def qux = {
                  success.filterWithStacktrace(_ < 0, "Foobar")
                }
                qux
              }
              baz
            }
            bar
          }

          val ex = foo.getFailure
          ex.getMessage shouldReturn "Foobar"
          ex.getStackTrace.zip(List("qux", "baz", "bar", "foo"))
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
  }
}
