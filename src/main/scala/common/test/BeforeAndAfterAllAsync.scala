package common.test

import org.scalatest.{Args, AsyncTestSuite, AsyncTestSuiteMixin, Status}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait BeforeAndAfterAllAsync extends AsyncTestSuiteMixin { self: AsyncTestSuite =>
  protected def beforeAll(): Future[_] = BeforeAndAfterAllAsync.EmptyFuture
  protected def afterAll(): Future[_] = BeforeAndAfterAllAsync.EmptyFuture
  abstract override def run(testName: Option[String], args: Args): Status = {
    Await.result(beforeAll(), Duration.Inf)
    val $ = super.run(testName, args)
    Await.result(afterAll(), Duration.Inf)
    $
  }
}

private object BeforeAndAfterAllAsync {
  private val EmptyFuture = Future.successful(())
}
