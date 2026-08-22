package common

import java.util.concurrent.{CountDownLatch, Executors}
import java.util.concurrent.atomic.AtomicInteger

import cats.implicits.toFunctorOps
import org.scalatest.Inspectors.forAll
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

import common.rich.func.kats.IteratorInstances.iteratorInstances

import common.rich.primitives.RichInt.Rich

class CacheMapTest extends AnyFreeSpec {
  "Concurrent applications invoke the underlying function once at most" in {
    val tp = Executors.newFixedThreadPool(8)
    val n = 4
    val counts = 0.until(n).iterator.fproduct(_ => new AtomicInteger).toMap
    val random = new Random(0)
    val $ = CacheMap[Int, Int] { i =>
      Thread.sleep(random.nextInt(10))
      counts(i).incrementAndGet()
      Thread.sleep(random.nextInt(10))
      i * i
    }
    val runs = 10000
    val latch = new CountDownLatch(runs)
    runs.times {
      tp.execute { () =>
        $(random.nextInt(n))
        latch.countDown()
      }
    }
    latch.await()
    forAll(0.until(n))(i => $.get(i).forall(_ == i * i))
  }
}
