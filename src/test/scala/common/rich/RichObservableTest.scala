package common.rich

import java.util.NoSuchElementException

import org.scalatest.AsyncFreeSpec
import rx.lang.scala.{Observable, Observer}
import rx.lang.scala.subjects.PublishSubject

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.language.postfixOps

import scalaz.std.scalaFuture.futureInstance
import scalaz.syntax.functor.ToFunctorOps

import common.AuxSpecs
import common.rich.RichObservable._

class RichObservableTest extends AsyncFreeSpec with AuxSpecs {
  private def createSubject = PublishSubject[Int]()
  "toFuture" - {
    "success" in {
      val sub = createSubject
      val future = sub.toFuture[Vector]
      sub.onNext(1)
      sub.onNext(2)
      sub.onNext(3)
      sub.onCompleted()
      future.map(_ shouldReturn Vector(1, 2, 3))
    }
    "failure" in {
      val sub = createSubject
      val error = new Exception("foobar")
      val future = sub.toFuture[Vector]
      sub.onNext(1)
      sub.onNext(2)
      sub.onNext(3)
      sub.onError(error)
      future.checkFailure(_ shouldReturn error)
    }
  }

  "firstFuture" - {
    "non empty" in {
      val sub = createSubject
      val future = sub.firstFuture
      sub.onNext(1)
      sub.onNext(2)
      sub.onNext(3)
      future.map(_ shouldReturn 1)
    }
    "empty" in {
      val sub = createSubject
      val future = sub.firstFuture
      sub.onCompleted()
      future.checkFailure(_ shouldBe a[NoSuchElementException])
    }
  }

  "flattenElements" in {
    val x: Observable[Int] = Observable.just(Vector(1, 2, 3, 4)).flattenElements
    x.toFuture[Vector].map(_ shouldReturn Vector(1, 2, 3, 4))
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
      }).>|(actual shouldReturn ArrayBuffer(1, 2, 3))
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
      }).checkFailure(_.shouldReturn(error) && actual.shouldReturn(ArrayBuffer(1, 2, 3)))
    }
  }

  "register" - {
    "explicit emitter" in {
      val sub = createSubject
      val $ = RichObservable.register(sub.foreach)
      val actual = ArrayBuffer[Int]()
      $.foreach(actual.+=)
      sub.onNext(1)
      sub.onNext(2)
      sub.onNext(3)
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
      RichObservable.concat(Vector[Observable[Int]]()).toFuture.map(_ shouldReturn Vector[Int]())
    }
    "simple" in {
      RichObservable.concat(Vector(Observable.just(1), Observable.just(2))).toFuture
          .map(_ shouldReturn Vector(1, 2))
    }
    "reusable" in {
      val v = Vector(Observable.just(1), Observable.just(2))
      val $ = RichObservable.concat(v)
      $.toFuture.map(_ shouldReturn Vector(1, 2))
      $.toFuture.map(_ shouldReturn Vector(1, 2))
    }
    "large list" in {
      RichObservable.concat(1.to(10000).map(Observable.just(_))).toFuture
          .map(_ shouldReturn 1.to(10000).toVector)
    }
    "unsubscribes" in {
      val source1 = createSubject
      val source2 = createSubject
      val buffer = ArrayBuffer[Int]()
      val sub = RichObservable.concat(Vector(source1, source2)).subscribe(buffer += _)
      source1.onNext(1)
      source2.onNext(2)
      sub.unsubscribe()
      source1.onNext(3)
      source2.onNext(4)
      Future {
        Thread sleep 200
        buffer shouldReturn ArrayBuffer(1, 2)
      }
    }
  }
}
