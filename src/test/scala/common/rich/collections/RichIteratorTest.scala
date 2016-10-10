package common.rich.collections

import java.util
import java.util.HashSet

import common.AuxSpecs
import common.rich.collections.RichIterator._
import org.scalatest.FlatSpec
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.matchers.ShouldMatchers

import scala.util.Random
import org.scalatest.time.SpanSugar._

class RichIteratorTest extends FlatSpec with AuxSpecs with TimeLimitedTests {
  val timeLimit = 1 second

  "verifyForAll" should "throw an exception if f is not satisfied" in {
    an[Exception] should be thrownBy {
      List(1, 2, 3).iterator.verify(_ < 3).toVector
    }
  }

  it should "not throw an exception if all passes" in {
    List(1, 2, 3).iterator.verify(_ < 4).toList shouldReturn List(1, 2, 3)
  }

  "zipWithIndex" should "zip with index" in {
    List(1.0, 2.0, 3.0).iterator.zipWithIndex.toList shouldReturn List((1.0, 0), (2.0, 1), (3.0, 2))
  }

  "par" should "create threads to run a map request" in {
    val set = new HashSet[Object]()
    val r = new Random()
    List.fill(50)(r.nextDouble).toIterator.par().foreach(e => set.synchronized {
      set.add(Thread.currentThread())
    })
    set.size should be > 1
  }

  it should "return the items in order" in {
    List(1, 2, 3, 4, 5, 6).iterator.par().map(_ * 2).toList shouldReturn List(2, 4, 6, 8, 10, 12)
  }

  it should "work on lists of size 10" in {
    (1 to 10).toList.iterator.par().map(_ * 2).toList shouldReturn (2 to 20 by 2).toList
  }

  it should "run work in parallel and be faster than a serialize execution" in {
    val list = (1 to 50).toList
    val serTime = time {
      list.iterator.foreach(e => Thread.sleep(1))
    }
    val parTime = time {
      list.iterator.par().foreach(e => Thread.sleep(1))
    }
    serTime.toDouble / parTime should be >= 1.5
  }

  it should "run maps in parallel" in {
    val list = (1 to 50).toList
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

  it should "work on range" in {
    val set = new util.HashSet[Int]()
    (1 to 100).iterator.par().foreach(set.synchronized(set.add(_)))
  }
}
