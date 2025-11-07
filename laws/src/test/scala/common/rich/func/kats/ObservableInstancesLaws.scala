package common.rich.func.kats

import cats.Eq
import cats.laws.discipline.MonadTests
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import rx.lang.scala.Observable

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

import common.rich.func.kats.ObservableInstances.observableInstances

import common.rich.RichFuture.richFuture
import common.rich.RichObservable.richObservable

class ObservableInstancesLaws extends AnyFunSuite with FunSuiteDiscipline with Configuration {
  implicit def arbObservable[A: Arbitrary]: Arbitrary[Observable[A]] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary.map(Observable.from(_)))

  implicit def observableEq[A: Eq]: Eq[Observable[A]] = (xI, yI) => {
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global
    xI.to[List].firstFuture.get === yI.to[List].firstFuture.get
  }

  checkAll("Observable.MonadLaws", MonadTests[Observable].monad[Int, Int, String])
}
