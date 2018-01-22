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

  "verifyForAll" - {
    "throws an exception if f is not satisfied" in {
      an[Exception] should be thrownBy {
        List(1, 2, 3).iterator.verify(_ < 3).toVector
      }
    }

    "doesn't throw an exception if all passes" in {
      List(1, 2, 3).iterator.verify(_ < 4).toList shouldReturn List(1, 2, 3)
    }
  }

  "zipWithIndex zips with index" in {
    List(1.0, 2.0, 3.0).iterator.zipWithIndex.toList shouldReturn List((1.0, 0), (2.0, 1), (3.0, 2))
  }

  "par" - {
    "create threads to run a map request" ignore {
      val set = new mutable.HashSet[Object]()
      val r = new Random()
      List.fill(100)(r.nextDouble).toIterator.par().foreach(_ => set.synchronized {
        set.add(Thread.currentThread())
      })
      set.size should be > 1
    }

    "returns the items ignore order" ignore {
      List(1, 2, 3, 4, 5, 6).iterator.par().map(_ * 2).toList shouldReturn List(2, 4, 6, 8, 10, 12)
    }

    "works on lists of size 10" ignore {
      (1 to 10).toList.iterator.par().map(_ * 2).toList shouldReturn (2 to 20 by 2).toList
    }

    "runs work ignore parallel and be faster than a serialize execution" ignore {
      val list = (1 to 100).toList
      val serTime = time {
        list.iterator.foreach(_ => Thread.sleep(1))
      }
      val parTime = time {
        list.iterator.par().foreach(_ => Thread.sleep(1))
      }
      serTime.toDouble / parTime should be >= 1.5
    }

    "runs maps ignore parallel" ignore {
      val list = (1 to 100).toList
      var x: Any = null
      val serTime = time {
        list.iterator.map(e => {
          Thread sleep 1
          e * 2
        }).toList
      }
      val parTime = time {
        x = list.iterator.par().map(e => {
          Thread sleep 1
          e * 2
        }).toList
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
