package common.rich.func

import org.scalatest.FreeSpec

import scalaz.std.vector.vectorInstance
import scalaz.Functor
import common.rich.func.MoreFunctorSyntax._

import common.AuxSpecs

object MoreFunctorSyntaxTest {
  private case class Box[A](a: A)
  private object Box {
    implicit object FunctorEv extends Functor[Box] {
      override def map[A, B](fa: Box[A])(f: A => B) = Box(f(fa.a))
    }
  }
}

class MoreFunctorSyntaxTest extends FreeSpec with AuxSpecs {
  import MoreFunctorSyntaxTest._

  "listen" in {
    var sum = 0
    val $: Box[Int] = Box(42).listen(sum += _)
    val y = $.a * 10
    sum shouldReturn 42
    y shouldReturn 420
  }
  "unzip" in {
    val (o1, o2): (Box[Int], Box[Int]) = Box(1 -> 2).unzip
    o1 shouldReturn Box(1)
    o2 shouldReturn Box(2)
  }
  "ifNone" in {
    Vector(None, Some(3), None).ifNone(42) shouldReturn Vector(42, 3, 42)
  }
  "when" in {
    Vector(true, false, true).when(whenTrue = 1, whenFalse = 2) shouldReturn Vector(1, 2, 1)
  }
}
