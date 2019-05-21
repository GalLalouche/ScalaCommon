package common.rich

import java.util.NoSuchElementException

import common.AuxSpecs
import common.rich.RichFuture._
import org.scalatest.{FreeSpec, OneInstancePerTest}
import rx.lang.scala.subjects.PublishSubject
import rx.lang.scala.{Observable, Subject}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global

class RichObservableTest extends FreeSpec with AuxSpecs with OneInstancePerTest {
  import RichObservable._

  private val sub = PublishSubject[Int]()
  "toFuture" in {
    val future = sub.toFuture[List]
    sub.onNext(1)
    sub.onNext(2)
    sub.onNext(3)
    sub.onCompleted()
    future.get shouldReturn List(1, 2, 3)
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

  "from" - {
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
      val sub = RichObservable.concat(Vector(source1, source2)).subscribe(buffer.+=(_))
      source1.onNext(1)
      source2.onNext(2)
      sub.unsubscribe()
      source1.onNext(3)
      source2.onNext(4)
      buffer shouldReturn ArrayBuffer(1, 2)
    }
  }
}
