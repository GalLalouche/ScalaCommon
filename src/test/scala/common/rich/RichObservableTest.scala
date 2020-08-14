package common.rich

import java.util.NoSuchElementException
import java.util.concurrent.LinkedBlockingQueue

import org.scalatest.AsyncFreeSpec
import rx.lang.scala.{Observable, Observer}
import rx.lang.scala.subjects.PublishSubject

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.language.postfixOps

import scalaz.syntax.functor.ToFunctorOps
import scalaz.OptionT
import common.rich.func.BetterFutureInstances._

import common.rich.RichObservable._
import common.test.AsyncAuxSpecs

class RichObservableTest extends AsyncFreeSpec with AsyncAuxSpecs {
  private def createSubject = PublishSubject[Int]()
  "richObservable" - {
    "toFuture" - {
      "success" in {
        val sub = createSubject
        val future = sub.toFuture[Vector]
        sub.onNext(1)
        sub.onNext(2)
        sub.onNext(3)
        sub.onCompleted()
        future shouldEventuallyReturn Vector(1, 2, 3)
      }
      "failure" in {
        val sub = createSubject
        val error = new Exception("foobar")
        val future = sub.toFuture[Vector]
        sub.onNext(1)
        sub.onNext(2)
        sub.onNext(3)
        sub.onError(error)
        future failureShouldEventuallyReturn error
      }
    }

    "firstFuture" - {
      "non empty" in {
        val sub = createSubject
        val future = sub.firstFuture
        sub.onNext(1)
        sub.onNext(2)
        sub.onNext(3)
        future shouldEventuallyReturn 1
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
      x.toFuture[Vector] shouldEventuallyReturn Vector(1, 2, 3, 4)
    }

    "subscribeWithNotification" - {
      "Does not complete" in {
        val o = Observable[Int] {obs =>
          obs.onNext(1)
          obs.onNext(2)
          obs.onNext(3)
        }
        val actual = ArrayBuffer[Int]()
        val f = o.subscribeWithNotification(new Observer[Int] {
          override def onNext(value: Int): Unit = actual += value
        })
        Thread sleep 200
        f.isCompleted shouldReturn false
        actual shouldReturn ArrayBuffer(1, 2, 3)
      }
      "Completes" in {
        val o = Observable[Int] {obs =>
          obs.onNext(1)
          obs.onNext(2)
          obs.onNext(3)
          obs.onCompleted()
        }
        val actual = ArrayBuffer[Int]()
        o.subscribeWithNotification(new Observer[Int] {
          override def onNext(value: Int): Unit = actual += value
        }).>|(actual shouldReturn ArrayBuffer(1, 2, 3))
      }
      "Failure" in {
        val error = new Exception("foobar")
        val o = Observable[Int] {obs =>
          obs.onNext(1)
          obs.onNext(2)
          obs.onNext(3)
          obs.onError(error)
        }
        val actual = ArrayBuffer[Int]()
        o.subscribeWithNotification(new Observer[Int] {
          override def onNext(value: Int): Unit = actual += value
        }).checkFailure(_.shouldReturn(error) && actual.shouldReturn(ArrayBuffer(1, 2, 3)))
      }
    }

    "doOnNextAsync" - {
      val e = new Exception("Error")
      "All valid" in {
        val sub = createSubject
        val queue = new LinkedBlockingQueue[Int](3)
        val future = sub.doOnNextAsync(e => Future(queue.add(e))).toFuture[Vector]
        sub.onNext(1)
        sub.onNext(2)
        sub.onNext(3)
        sub.onCompleted()
        future.map(_.shouldReturn(Vector(1, 2, 3)) &&
            queue.asScala.toVector.shouldReturn(Vector(1, 2, 3))
        )
      }
      "Should not proceed on synced error, and return the correct error" in {
        runAndVerifyError {(i, queue) =>
          i match {
            case e if e < 3 => Future(queue.add(e))
            case _ => throw e
          }
        }
      }
      "Should not proceed on unsynced error, and return the correct error" in {
        runAndVerifyError {(i, queue) =>
          i match {
            case e if e < 3 => Future(queue.add(e))
            case _ => Future failed e
          }
        }
      }

      def runAndVerifyError(f: (Int, LinkedBlockingQueue[Int]) => Future[_]) = {
        val sub = createSubject
        val queue = new LinkedBlockingQueue[Int](3)
        val future = sub.doOnNextAsync(f(_, queue)).toFuture
        sub.onNext(1)
        sub.onNext(2)
        sub.onNext(3)
        for {
          failure <- future.checkFailure(_ shouldBe theSameInstanceAs(e))
          res <- failure && (queue.asScala.toVector shouldReturn Vector(1, 2))
        } yield res
      }
    }

    "filterFuture" in {
      Observable.just(1, 2, 3).filterFuture(i => Future.successful(i % 2 == 0))
          .toFuture[Vector] shouldEventuallyReturn Vector(2)
    }

    "mapFutureOption" in {
      Observable.just(1, 2, 3)
          .mapFutureOption[String](i => if (i % 2 == 0) OptionT.some(i.toString) else OptionT.none)
          .toFuture[Vector] shouldEventuallyReturn Vector("2")
    }
  }

  "Object methods" - {
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
        subscribers shouldBe empty
        val s = $.subscribe(_ => ???)
        subscribers.size shouldReturn 1
        s.unsubscribe()
        subscribers shouldBe empty
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
      subscribers shouldBe empty
      val s = $.subscribe(_ => ???)
      subscribers.size shouldReturn 1
      s.unsubscribe()
      subscribers shouldBe empty
    }

    "concat" - {
      "empty" in {
        RichObservable.concat(Vector[Observable[Int]]()).toFuture shouldEventuallyReturn Vector.empty
      }
      "simple" in {
        RichObservable.concat(Vector(Observable.just(1), Observable.just(2)))
            .toFuture shouldEventuallyReturn Vector(1, 2)
      }
      "reusable" in {
        val v = Vector(Observable.just(1), Observable.just(2))
        val $ = RichObservable.concat(v)
        $.toFuture shouldEventuallyReturn Vector(1, 2)
        $.toFuture shouldEventuallyReturn Vector(1, 2)
      }
      "large list" in {
        RichObservable.concat(1.to(10000).map(Observable.just(_)))
            .toFuture shouldEventuallyReturn 1.to(10000).toVector
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
}
