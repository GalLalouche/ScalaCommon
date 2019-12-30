package common.storage

import org.scalatest.{AsyncFreeSpec, OneInstancePerTest}
import org.scalatest.OptionValues._

import scala.collection.mutable
import scala.concurrent.Future

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.bind._

import common.test.AsyncAuxSpecs

class StorageTemplateTest extends AsyncFreeSpec with OneInstancePerTest with AsyncAuxSpecs {
  private val existingValues = mutable.HashMap[Int, Int]()
  private val $ = new StorageTemplate[Int, Int]() {
    override protected def internalDelete(k: Int) = Future successful existingValues.remove(k)
    override protected def internalForceStore(k: Int, v: Int) = {
      existingValues += k -> v
      Future successful Unit
    }
    override def load(k: Int): Future[Option[Int]] = Future successful existingValues.get(k)
    override def storeMultiple(kvs: Seq[(Int, Int)]) =
      if ((existingValues.keySet & kvs.map(_._1).toSet).nonEmpty)
        Future failed new IllegalArgumentException
      else
        Future successful existingValues.++=(kvs)
    override def utils = ???
  }
  "store" - {
    "has existing value throws" in {
      existingValues += 1 -> 2
      $.store(1, 4).shouldFail() >| (existingValues(1) shouldReturn 2)
    }
    "no existing value should insert the value and return true" in {
      $.store(1, 4) >| (existingValues(1) shouldReturn 4)
    }
  }
  "forceStore" - {
    "has existing value returns old value" in {
      existingValues += 1 -> 2
      $.forceStore(1, 4).map(_ shouldReturn Some(2)) >|
          (existingValues(1) shouldReturn 4)
    }
    "has no existing value returns None" in {
      $.forceStore(1, 4).map(_ shouldReturn None) >|
          (existingValues(1) shouldReturn 4)
    }
  }
  "mapStore" - {
    "has no existing value uses default" in {
      $.mapStore(1, _ => ???, 2).map(_ shouldReturn None) >|
          (existingValues(1) shouldReturn 2)
    }
    "maps existing value if present" in {
      existingValues += 1 -> 2
      $.mapStore(1, _ * 2, ???).map(_ shouldReturn Option(2)) >|
          (existingValues(1) shouldReturn 4)
    }
  }
  "delete" - {
    "existing value" in {
      existingValues += 1 -> 2
      $.delete(1).map(_.value shouldReturn 2) >>
          $.load(1).map(_ shouldReturn None)
    }
    "no existing value" in {
      $.delete(1).map(_ shouldReturn None) >>
          $.load(1).map(_ shouldReturn None)
    }
  }
}
