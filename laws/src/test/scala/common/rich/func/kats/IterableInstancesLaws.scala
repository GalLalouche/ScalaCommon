package common.rich.func.kats

import cats.Eq
import cats.implicits.catsSyntaxEq
import cats.laws.discipline.MonadTests
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import common.rich.func.kats.MoreIterableInstances.iterableInstances

import common.rich.primitives.RichBoolean.richBoolean

class IterableInstancesLaws extends AnyFunSuite with FunSuiteDiscipline with Configuration {
  implicit def arbIterable[A: Arbitrary]: Arbitrary[Iterable[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary)

  implicit def iterableEqv[A: Eq]: Eq[Iterable[A]] = new Eq[Iterable[A]] {
    override def eqv(xI: Iterable[A], yI: Iterable[A]): Boolean = {
      val x = xI.iterator
      val y = yI.iterator
      while (x.hasNext && y.hasNext)
        if (x.next().eqv(y.next()).isFalse)
          return false
      x.hasNext == y.hasNext
    }
  }

  checkAll("Iterable.MonadLaws", MonadTests[Iterable].monad[Int, Int, String])
}
