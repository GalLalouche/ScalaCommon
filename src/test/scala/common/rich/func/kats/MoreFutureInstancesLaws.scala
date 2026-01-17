package common.rich.func.kats

import java.util.concurrent.Executors

import cats.Eq
import cats.laws.discipline.CommutativeMonadTests
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalatest.tags.Slow
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

import common.rich.func.kats.MoreFutureInstances.futureIsCommutativeMonad

import common.rich.RichFuture.richFutureBlocking

@Slow
class MoreFutureInstancesLaws extends AnyFunSuite with FunSuiteDiscipline with Configuration {
  private implicit val iec: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  implicit def arbFuture[A: Arbitrary]: Arbitrary[Future[A]] =
    Arbitrary(implicitly[Arbitrary[A]].arbitrary.map(Future(_)))

  implicit def futureEq[A: Eq]: Eq[Future[A]] = (x: Future[A], y: Future[A]) => x.get === y.get

  checkAll(
    "Future.CommutativeMonadLaws",
    CommutativeMonadTests[Future].commutativeMonad[Int, Int, String],
  )
}
