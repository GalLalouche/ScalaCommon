package common.rich.func.kats

import java.util.concurrent.Executors

import cats.{Eval, Functor}
import cats.implicits.toFunctorOps
import org.scalatest.freespec.AsyncFreeSpec

import scala.concurrent.{ExecutionContext, Future}

import common.rich.func.kats.ToSequentialTraverseOps.sequentialTraverseOps

import common.test.AsyncAuxSpecs

class SequentialTraverseTest extends AsyncFreeSpec with AsyncAuxSpecs {
  implicit override def executionContext: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(3))
  def test[F[_]: SequentialTraverse: Functor](range: F[Int]): Unit = {
    "Does not blow the stack" in {
      range.mapM(i => Eval.later(i * 10)).value shouldReturn range.map(_ * 10)
    }
    "is sequential" in {
      var current = 0
      range
        .mapM(i =>
          Future {
            assert(i == current)
            current += 1
            i * 10
          },
        ) shouldEventuallyReturn range.map(_ * 10)
    }
  }
  "instances" - {
    "List" - test(List.range(0, 1000))
    "Vector" - test(Vector.range(0, 1000))
  }
}
