package common.concurrency

import java.util.concurrent.{Semaphore, TimeUnit}
import common.rich.RichT._

import common.AuxSpecs
import org.scalatest.{FreeSpec, OneInstancePerTest}

import scala.concurrent.ExecutionContext

class LazyTest extends FreeSpec with AuxSpecs with OneInstancePerTest {
  private val lock = new Semaphore(0)
  private implicit val ec: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit = {
      val $ = new Thread(runnable)
      $ setDaemon true
      $.start()
    }
    override def reportFailure(cause: Throwable): Unit = ()
  }
  var counter = 0

  "apply is lazy" - {
    "map and flatMap" in {
      val $ = Lazy {
        counter += 1
        1
      }.map(x => {
        counter += 1
        x + 1
      }).flatMap(x => {
        counter += 1
        Lazy {
          counter += 1
          x + 1
        }
      })
      counter shouldReturn 0
      $.join() shouldReturn 3
      counter shouldReturn 4
    }
    "filter is memoized" in {
      val $ = Lazy {
        counter += 1
        1
      }.filter(false.const)
      an[Exception] should be thrownBy $.join()
      an[Exception] should be thrownBy $.join()
      counter shouldReturn 1
    }
    "filter and orElse" in {
      val $ = Lazy {
        counter += 1
        1
      }.filter(false.const)
      counter shouldReturn 0
      an[Exception] should be thrownBy $.join()
      counter shouldReturn 1
      val $2 = $ orElse Lazy {
        counter += 1
        42
      }
      counter shouldReturn 1
      $2.join() shouldReturn 42
      counter shouldReturn 2
    }
  }

  "async is eager" - {
    "ctor" in {
      Lazy.async {
        lock.release()
        1
      }
      lock.tryAcquire(1, TimeUnit.SECONDS) shouldReturn true
    }
    "map" in {
      Lazy.async {
        lock.release()
        ()
      }.map {_ =>
        lock.release()
        ()
      }
      lock.tryAcquire(2, 1, TimeUnit.SECONDS) shouldReturn true
    }
    "flatMap" in {
      Lazy.async {
        lock.release()
        ()
      }.flatMap {_ =>
        lock.release()
        Lazy()
      }
      lock.tryAcquire(2, 1, TimeUnit.SECONDS) shouldReturn true
    }
    "flatMap is eager even if its input isn't" in {
      Lazy.async {
        lock.release()
        ()
      }.flatMap {_ =>
        lock.release()
        Lazy()
      }.map {_ =>
        lock.release()
        ()
      }
      lock.tryAcquire(3, 1, TimeUnit.SECONDS) shouldReturn true
    }
    "orElse" - {
      "eager on failure" in {
        Lazy.async {
          lock.release()
          ()
        }.filter(false.const) orElse Lazy {
          lock.release()
          ()
        }
        lock.tryAcquire(2, 1, TimeUnit.SECONDS) shouldReturn true
      }
      "but lazy otherwise" in {
        Lazy.async {
          lock.release()
          ()
        } orElse Lazy {
          lock.release()
          ()
        }
        lock.tryAcquire(1, 1, TimeUnit.SECONDS) shouldReturn true
        Thread.sleep(1000)
        lock.availablePermits() shouldReturn 0
      }
    }
  }
}
