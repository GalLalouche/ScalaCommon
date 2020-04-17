package common.rich.func

import org.scalatest.FreeSpec

import scalaz.{Monad, Writer}
import scalaz.std.string.stringInstance
import scalaz.WriterT.tell
import common.rich.func.ToMoreMonadOps._

import common.test.AuxSpecs

private object ToMoreMonadOpsTest {
  private case class Box[+A](a: A)

  private implicit object BoxMonad extends Monad[Box] {
    override def point[A](a: => A): Box[A] = Box(a)
    override def bind[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = f(fa.a)
  }
}

class ToMoreMonadOpsTest extends FreeSpec with AuxSpecs {
  import common.rich.func.ToMoreMonadOpsTest.Box

  private val none: Box[Option[Int]] = Box(None)
  private val some: Box[Option[Int]] = Box(Some(42))
  "mFilterOpt" - {
    "None remains None" in {
      none.mFilterOpt(_ => ???).a shouldReturn None
    }
    "Some" - {
      "Pred is true returns self" in {
        some.mFilterOpt(e => Box(e % 2 == 0)).a shouldReturn Some(42)
      }
      "Pred is false returns None" in {
        some.mFilterOpt(e => Box(e % 2 == 1)).a shouldReturn None
      }
    }
  }
  "ifNoneTry" - {
    "Some" in {
      some.ifNoneTry(???).a shouldReturn 42
    }
    "None" in {
      none.ifNoneTry(Box(42)).a shouldReturn 42
    }
  }

  "conditionals" - {
    type StringWriter[A] = Writer[String, A]
    val writer: Writer[String, Unit] = tell("foo")
    def wrap(b: Boolean) = Monad[StringWriter].point(b)
    "ifTrue" - {
      "true" in {wrap(true).ifTrue(writer).run._1 shouldReturn "foo"}
      "false" in {wrap(false).ifTrue(???).run._1 shouldReturn ""}
    }
    "ifFalse" - {
      "true" in {wrap(true).ifFalse(???).run._1 shouldReturn ""}
      "false" in {wrap(false).ifFalse(writer).run._1 shouldReturn "foo"}
    }
  }
}
