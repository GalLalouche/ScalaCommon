package common.test

import org.scalatest.{Args, AsyncTestSuite, AsyncTestSuiteMixin, Status}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

trait BeforeAndAfterAllAsync extends AsyncTestSuiteMixin { self: AsyncTestSuite =>
  // The default ExecutionContext implementation is a package-private type that doesn't actually run anything!
  // Since we can't block on the result of beforeAll(), and we have no way of actually running the damn thing,
  // if you want to this trait you have to provide your own ExecutionContext, which does actually run things.
  implicit override def executionContext: ExecutionContext = throw new Exception(
    "The default ExecutionContext provided by scalatest is not supported when using BeforeAndAfterAllAsync. " +
      "Please override this function with an explicit ExecutionContext.",
  )
  protected def beforeAll(): Future[_] = BeforeAndAfterAllAsync.EmptyFuture
  protected def afterAll(): Future[_] = BeforeAndAfterAllAsync.EmptyFuture
  abstract override def run(testName: Option[String], args: Args): Status = {
    Await.result(beforeAll(), Duration.Inf)
    val $ = super.run(testName, args)
    $.whenCompleted(_ => Await.result(afterAll(), Duration.Inf))
    $
  }
}

private object BeforeAndAfterAllAsync {
  private val EmptyFuture = Future.successful(())
}
