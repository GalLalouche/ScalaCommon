package common.rich
import java.util.NoSuchElementException

import common.AuxSpecs
import org.scalatest.{FreeSpec, OneInstancePerTest}
import rx.lang.scala.subjects.PublishSubject

import scala.concurrent.ExecutionContext.Implicits.global
import RichFuture._
import rx.lang.scala.Observable

import scala.collection.mutable.ArrayBuffer

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
      future.getFailure shouldBe a [NoSuchElementException]
    }
  }
  "flattenElements" in {
    val x: Observable[Int] = Observable.just(List(1, 2, 3, 4)).flattenElements
    val actual = ArrayBuffer[Int]()
    x.doOnNext(actual.+=).subscribe()
    actual shouldReturn ArrayBuffer(1, 2, 3, 4)
  }
}
