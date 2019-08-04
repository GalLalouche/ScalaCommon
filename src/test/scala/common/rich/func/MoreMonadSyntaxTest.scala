package common.rich.func

import org.scalatest.FreeSpec

import scalaz.Monad
import common.rich.func.MoreMonadSyntax._

import common.AuxSpecs

private object MoreMonadSyntaxTest {
  private case class Box[+A](a: A)

  private implicit object BoxMonad extends Monad[Box] {
    override def point[A](a: => A): Box[A] = Box(a)
    override def bind[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = f(fa.a)
  }
}

class MoreMonadSyntaxTest extends FreeSpec with AuxSpecs {
  import common.rich.func.MoreMonadSyntaxTest.Box

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
}
