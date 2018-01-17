package common.rich.func

import common.AuxSpecs
import org.scalatest.FreeSpec

import scalaz.Monad
import scalaz.std.OptionInstances

private object ToMoreMonadOpsTest {
  private case class Box[+A](a: A)

  private implicit object BoxMonad extends Monad[Box] {
    override def point[A](a: => A): Box[A] = Box(a)
    override def bind[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = f(fa.a)
  }
}

class ToMoreMonadOpsTest extends FreeSpec with AuxSpecs
    with OptionInstances with ToMoreMonadOps {
  import common.rich.func.ToMoreMonadOpsTest.Box
  "mFilterOpt" - {
    "None remains None" in {
      val none: Box[Option[Int]] = Box(None)
      none.mFilterOpt(_ => ???).a shouldReturn None
    }
    "Some" - {
      val some: Box[Option[Int]] = Box(Some(42))
      "Pred is true returns self" in {
        some.mFilterOpt(e => Box(e % 2 == 0)).a shouldReturn Some(42)
      }
      "Pred is false returns None" in {
        some.mFilterOpt(e => Box(e % 2 == 1)).a shouldReturn None
      }
    }
  }
}
