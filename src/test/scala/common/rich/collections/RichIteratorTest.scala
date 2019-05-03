package common.rich.collections

import java.util

import common.AuxSpecs
import common.rich.collections.RichIterator._
import org.scalatest.FreeSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class RichIteratorTest extends FreeSpec with AuxSpecs with TimeLimitedTests {
  override val timeLimit = 2 seconds
  private def compareIterators[T](actual: Iterator[T], expected: TraversableOnce[T]): Unit =
    actual.toVector shouldReturn expected.toVector

  "verifyForAll" - {
    "throws an exception if f is not satisfied" in {
      an[Exception] should be thrownBy {
        Iterator(1, 2, 3).verify(_ < 3).toVector
      }
    }

    "doesn't throw an exception if all passes" in {
      compareIterators(Iterator(1, 2, 3).verify(_ < 4), Iterator(1, 2, 3))
    }
  }

  "zipWithIndex zips with index" in {
    compareIterators(Iterator(1.0, 2.0, 3.0).zipWithIndex, Vector((1.0, 0), (2.0, 1), (3.0, 2)))
  }

  "reducingIterator" - {
    "empty" in {
      Iterator[Int]().reducingIterator((_, _) => ???) shouldBe 'empty
    }
    "single element" in {
      compareIterators(Iterator(1).reducingIterator(_ + _), Vector(1))
    }
    "two elements" in {
      compareIterators(Iterator(1, 2).reducingIterator(_ + _), Vector(1, 3))
    }
    "five elements" in {
      compareIterators(Iterator(1, 2, 3, 4, 5).reducingIterator(_ + _), Vector(1, 3, 6, 10, 15))
    }
  }

  "takeUntil" - {
    "empty" in {
      Iterator[Int]().takeUntil(_ => ???) shouldBe 'empty
    }
    "all true" in {
      compareIterators(Iterator(1, 2, 3).takeUntil(_ < 4), Vector(1, 2, 3))
    }
    "first element is false" in {
      compareIterators(Iterator(1, 2, 3).takeUntil(_ != 1), Vector(1))
    }
    "second element is false" in {
      compareIterators(Iterator(1, 2, 3).takeUntil(_ != 2), Vector(1, 2))
    }
  }

  "par" ignore {
    "create threads to run a map request" ignore {
      val set = new mutable.HashSet[Object]()
      val r = new Random()
      Iterator.fill(100)(r.nextDouble).par().foreach(_ => set.synchronized {
        set.add(Thread.currentThread())
      })
      set.size should be > 1
    }

    "returns the items ignore order" ignore {
      Iterator(1, 2, 3, 4, 5, 6).par().map(_ * 2).toVector shouldReturn Vector(2, 4, 6, 8, 10, 12)
    }

    "works on lists of size 10" ignore {
      (1 to 10).iterator.par().map(_ * 2).toVector shouldReturn (2 to 20 by 2).toVector
    }

    "runs work ignore parallel and be faster than a serialize execution" ignore {
      val list = (1 to 100).toVector
      val serTime = time {
        list.iterator.foreach(_ => Thread.sleep(1))
      }
      val parTime = time {
        list.iterator.par().foreach(_ => Thread.sleep(1))
      }
      serTime.toDouble / parTime should be >= 1.5
    }

    "runs maps ignore parallel" ignore {
      val list = (1 to 100).toVector
      var x: Any = null
      val serTime = time {
        list.iterator.map(e => {
          Thread sleep 1
          e * 2
        }).toVector
      }
      val parTime = time {
        x = list.iterator.par().map(e => {
          Thread sleep 1
          e * 2
        }).toVector
      }
      serTime.toDouble / parTime should be >= 1.5
      x shouldReturn (list map (_ * 2))
    }

    "works on range" ignore {
      val set = new util.HashSet[Int]()
      (1 to 100).iterator.par().foreach(set.synchronized(set.add(_)))
    }
  }
}
