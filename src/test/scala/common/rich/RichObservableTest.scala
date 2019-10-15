package common.rich

import java.util.NoSuchElementException

import org.scalatest.{FreeSpec, OneInstancePerTest}
import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimitedTests}
import org.scalatest.time.SpanSugar._
import rx.lang.scala.{Observable, Observer, Subject}
import rx.lang.scala.subjects.PublishSubject

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

import common.AuxSpecs
import common.rich.RichFuture._
import common.rich.RichObservable._

class RichObservableTest extends FreeSpec with AuxSpecs with OneInstancePerTest with TimeLimitedTests {
  override val timeLimit = 1 seconds
  override val defaultTestSignaler: Signaler = ThreadSignaler

  private val sub = PublishSubject[Int]()
  "toFuture" - {
    "success" in {
      val future = sub.toFuture[List]
      sub.onNext(1)
      sub.onNext(2)
      sub.onNext(3)
      sub.onCompleted()
      future.get shouldReturn List(1, 2, 3)
    }
    "failure" in {
      val error = new Exception("foobar")
      val future = sub.toFuture[List]
      sub.onNext(1)
      sub.onNext(2)
      sub.onNext(3)
      sub.onError(error)
      future.getFailure shouldReturn error
    }
  }

  "firstFuture" - {
    "non empty" in {
      val future = sub.firstFuture
      sub.onNext(1)
      sub.onNext(2)
      sub.onNext(3)
      future.get shouldReturn 1
    }
    "empty" in {
      val future = sub.firstFuture
      sub.onCompleted()
      future.getFailure shouldBe a[NoSuchElementException]
    }
  }

  "flattenElements" in {
    val x: Observable[Int] = Observable.just(List(1, 2, 3, 4)).flattenElements
    val actual = ArrayBuffer[Int]()
    x.doOnNext(actual.+=).subscribe()
    actual shouldReturn ArrayBuffer(1, 2, 3, 4)
  }

  "subscribeWithNotification" - {
    "Does not complete" in {
      val o = Observable[Int](obs => {
        obs.onNext(1)
        obs.onNext(2)
        obs.onNext(3)
      })
      val actual = ArrayBuffer[Int]()
      val f = o.subscribeWithNotification(new Observer[Int] {
        override def onNext(value: Int): Unit = actual += value
      })
      Thread sleep 200
      f.isCompleted shouldReturn false
      actual shouldReturn ArrayBuffer(1, 2, 3)
    }
    "Completes" in {
      val o = Observable[Int](obs => {
        obs.onNext(1)
        obs.onNext(2)
        obs.onNext(3)
        obs.onCompleted()
      })
      val actual = ArrayBuffer[Int]()
      o.subscribeWithNotification(new Observer[Int] {
        override def onNext(value: Int): Unit = actual += value
      }).get
      actual shouldReturn ArrayBuffer(1, 2, 3)
    }
    "Failure" in {
      val error = new Exception("foobar")
      val o = Observable[Int](obs => {
        obs.onNext(1)
        obs.onNext(2)
        obs.onNext(3)
        obs.onError(error)
      })
      val actual = ArrayBuffer[Int]()
      o.subscribeWithNotification(new Observer[Int] {
        override def onNext(value: Int): Unit = actual += value
      }).getFailure shouldReturn error
      actual shouldReturn ArrayBuffer(1, 2, 3)
    }
  }

  "register" - {
    "explicit emitter" in {
      val source = Subject[Int]()
      val $ = RichObservable.register(source.foreach)
      val actual = ArrayBuffer[Int]()
      $.foreach(actual.+=)
      source.onNext(1)
      source.onNext(2)
      source.onNext(3)
      actual shouldReturn ArrayBuffer(1, 2, 3)
    }
    "lazy emitter" in {
      val source = Observable.just(1, 2, 3)
      val $ = RichObservable.register(source.foreach)
      val actual = ArrayBuffer[Int]()
      $.foreach(actual.+=)
      actual shouldReturn ArrayBuffer(1, 2, 3)
    }
    "lazy subscribe and unsubscribe" in {
      val subscribers = ArrayBuffer[Any]()
      val $ = RichObservable.register[Any](subscribers.+=(_), () => subscribers.remove(0))
      subscribers shouldBe 'empty
      val s = $.subscribe(_ => ???)
      subscribers.size shouldReturn 1
      s.unsubscribe()
      subscribers shouldBe 'empty
    }
  }

  "registerUnsubscribable" in {
    val subscribers = ArrayBuffer[Any]()
    class Subscription {
      def unsubscribe = subscribers.remove(0)
    }
    implicit val ev: Unsubscribable[Subscription] = (a: Subscription) => a.unsubscribe
    val $ = RichObservable.registerUnsubscribable[Any, Subscription] {f =>
      subscribers.+=(f)
      new Subscription
    }
    subscribers shouldBe 'empty
    val s = $.subscribe(_ => ???)
    subscribers.size shouldReturn 1
    s.unsubscribe()
    subscribers shouldBe 'empty
  }

  "concat" - {
    "empty" in {
      RichObservable.concat(Vector[Observable[Int]]()).toFuture.get shouldReturn Vector[Int]()
    }
    "simple" in {
      RichObservable.concat(Vector(Observable.just(1), Observable.just(2))).toFuture.get shouldReturn Vector(1, 2)
    }
    "reusable" in {
      val v = Vector(Observable.just(1), Observable.just(2))
      val $ = RichObservable.concat(v)
      $.toFuture.get shouldReturn Vector(1, 2)
      $.toFuture.get shouldReturn Vector(1, 2)
    }
    "large list" in {
      RichObservable.concat(1.to(10000).map(Observable.just(_))).toFuture.get shouldReturn 1.to(10000).toVector
    }
    "unsubscribe" in {
      val source1 = PublishSubject[Int]()
      val source2 = PublishSubject[Int]()
      val buffer = ArrayBuffer[Int]()
      val sub = RichObservable.concat(Vector(source1, source2)).subscribe(buffer += _)
      source1.onNext(1)
      source2.onNext(2)
      sub.unsubscribe()
      source1.onNext(3)
      source2.onNext(4)
    }
  }
}
