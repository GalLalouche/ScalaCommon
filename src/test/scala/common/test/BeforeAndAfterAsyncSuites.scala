package common.test

import java.util.concurrent.Executors

import org.scalatest.{Args, Assertion, AsyncFreeSpec, FreeSpec, Suite}

import scala.concurrent.{ExecutionContext, Future}

import common.rich.primitives.RichBoolean._

class BeforeAndAfterAsyncSuites extends FreeSpec with AuxSpecs {
  private def run[A <: Suite](ms: A): ms.type = {
    ms.run(None, Args(StubReporter)).succeeds() shouldReturn true
    ms
  }

  "before and after each should be called before every test" in {
    class MySuite extends AsyncFreeSpec with BeforeAndAfterEachAsync with AuxSpecs {
      var setup = false
      var tearDown = false
      var beforeCalled = 0
      var afterCalled = 0

      protected override def beforeEach(): Future[_] = Future.successful {
        beforeCalled += 1
        setup = true
        tearDown = false
      }
      protected override def afterEach(): Future[_] = Future.successful {
        afterCalled += 1
        tearDown = true
        setup = false
      }

      private def validate(): Assertion = {
        setup shouldReturn true
        tearDown shouldReturn false
      }

      "test me1" in validate()
      "test me2" in validate()
    }
    val $ = run(new MySuite())
    $.setup shouldReturn false
    $.tearDown shouldReturn true
    $.beforeCalled shouldReturn 2
    $.afterCalled shouldReturn 2
  }

  "before and after all should be called just once, and only after all tests completed" in {
    class MySuite extends AsyncFreeSpec with BeforeAndAfterAllAsync with AuxSpecs {
      implicit override def executionContext: ExecutionContext =
        ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
      var setup = false
      var tearDown = false
      var beforeCalled = 0
      var afterCalled = 0

      protected override def beforeAll(): Future[_] = Future.successful {
        beforeCalled += 1
        setup = true
        tearDown = false
      }
      protected override def afterAll(): Future[_] = Future.successful {
        afterCalled += 1
        tearDown = true
        setup = false
      }

      private def validate() = {
        setup shouldReturn true
        tearDown shouldReturn false
      }
      "test me1" in validate()
      "test me2" in validate()
      "test me1 async" in Future(validate())
      "test me2 async" in Future(validate())
    }
    val $ = run(new MySuite())
    $.setup shouldReturn false
    $.tearDown shouldReturn true
    $.beforeCalled shouldReturn 1
    $.afterCalled shouldReturn 1
  }

  "composed with both" in {
    class MySuite
        extends AsyncFreeSpec
        with BeforeAndAfterAllAsync
        with BeforeAndAfterEachAsync
        with AuxSpecs {
      var setup = false
      var tearDown = false
      var setupAll = false
      var tearDownAll = false
      var beforeEachCalled = 0
      var afterEachCalled = 0
      var beforeAllCalled = 0
      var afterAllCalled = 0

      protected override def beforeEach(): Future[_] = Future.successful {
        assert(setupAll)
        beforeEachCalled += 1
        setup = true
        tearDown = false
      }
      protected override def afterEach(): Future[_] = Future.successful {
        assert(tearDownAll.isFalse)
        afterEachCalled += 1
        tearDown = true
        setup = false
      }
      protected override def beforeAll(): Future[_] = Future.successful {
        beforeAllCalled += 1
        setupAll = true
        tearDownAll = false
      }
      protected override def afterAll(): Future[_] = Future.successful {
        afterAllCalled += 1
        tearDownAll = true
        setupAll = false
      }
      private def validate() = {
        setup shouldReturn true
        tearDown shouldReturn false
        setupAll shouldReturn true
        tearDownAll shouldReturn false
      }
      "test me1" in validate()
      "test me2" in validate()
    }
    val $ = run(new MySuite())
    $.setup shouldReturn false
    $.tearDown shouldReturn true
    $.setupAll shouldReturn false
    $.tearDownAll shouldReturn true
    $.beforeAllCalled shouldReturn 1
    $.afterAllCalled shouldReturn 1
    $.beforeEachCalled shouldReturn 2
    $.afterEachCalled shouldReturn 2
  }
}
