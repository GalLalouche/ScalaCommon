package common.test

import org.scalatest.{AsyncTestSuite, AsyncTestSuiteMixin, FutureOutcome}

import scala.concurrent.Future

trait BeforeAndAfterEachAsync extends AsyncTestSuiteMixin { this: AsyncTestSuite =>
  protected def beforeEach(): Future[_] = BeforeAndAfterEachAsync.EmptyFuture
  protected def afterEach(): Future[_] = BeforeAndAfterEachAsync.EmptyFuture
  override def withFixture(test: NoArgAsyncTest): FutureOutcome =
    new FutureOutcome(for {
      _ <- beforeEach()
      result <- test().toFuture
      _ <- afterEach()
    } yield result)
}

private object BeforeAndAfterEachAsync {
  private val EmptyFuture = Future.successful(())
}
