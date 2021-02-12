package common.rich.func

import org.scalatest.{Assertion, AsyncFreeSpec}

import scala.concurrent.Future

import scalaz.{OptionT, StreamT}
import common.rich.func.BetterFutureInstances._
import common.rich.func.ToMoreMonadTransOps.toMoreMonadTransOps

import common.rich.RichT.richT
import common.test.AsyncAuxSpecs

class RichStreamTTest extends AsyncFreeSpec with AsyncAuxSpecs {
  "richStreamT" - {
    import RichStreamT._
    // Checks type inference too!
    val infiniteStream: StreamT[Future, Int] = RichStreamT.fromStream(Stream.iterate(1)(_ + 1))
    "oMapM" - {
      "empty" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](Nil)
            .oMapM[Int](_ => ???) valueShouldEventuallyReturn Stream.empty
      }
      def cubeEvens(e: Int) =
        if (e % 2 == 0)
          RichOptionT.pointSome[Future].apply(e * e * e)
        else
          OptionT.none[Future, Int]
      "multiple elements" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](List(1, 2, 3, 4))
            .oMapM(cubeEvens) valueShouldEventuallyReturn Stream(8, 64)
      }
      "infinite" in {
        infiniteStream.oMapM(cubeEvens)
            .take(3) valueShouldEventuallyReturn Stream(8, 64, 216)
      }
    }

    "subFlatMap" in {
      infiniteStream
          .tail
          .subFlatMap(e => Stream.iterate(e)(_ * e))
          .take(4) valueShouldEventuallyReturn Stream(2, 4, 8, 16)
    }

    "unconsBatch" - {
      def checkBoth(
          $: StreamT[Future, Int],
          n: Int,
          initAssertion: Seq[Int] => Assertion,
          tailAssertion: Seq[Int] => Assertion,
      ): Future[Assertion] = for {
        (b, next) <- $.unconsBatch(n)
        nextStream <- next.toStream
      } yield initAssertion(b) && tailAssertion(nextStream)
      "returns empty when empty" in {
        checkBoth(StreamT.empty[Future, Int], 10, _ shouldBe 'empty, _ shouldBe 'empty)
      }
      "returns empty when n = 0" in {
        checkBoth(RichStreamT.fromEvaluatedIterable(0.to(10)), 0, _ shouldBe 'empty, _ shouldReturn 0.to(10))
      }
      "Same as uncons when n = 1" in {
        checkBoth(RichStreamT.fromEvaluatedIterable(0.to(10)), 1, _.shouldContainExactly(0), _ shouldReturn 1.to(10))
      }
      "non trivial n" in {
        checkBoth(RichStreamT.fromEvaluatedIterable(0.to(10)), 5, _ shouldReturn 0.to(4), _ shouldReturn 5.to(10))
      }
    }
  }

  "Object methods" - {
    "iterateM" - {
      "basic" in {
        RichStreamT.iterateM(1)(e => RichOptionT.when(e < 5)(Future successful e + 1))
            .toStream shouldEventuallyReturn 1.to(5).toStream
      }
      "infinite" in {
        RichStreamT.iterateM(1)(e => RichOptionT.pointSome[Future].apply(e + 1)).take(5)
            .toStream shouldEventuallyReturn 1.to(5).toStream
      }
    }

    "fillM" - {
      "empty if the first element is None" in {
        RichStreamT.fillM(OptionT.none[Future, Int]).toStream shouldEventuallyReturn Stream.empty
      }
      "basic" in {
        var x = 1
        RichStreamT.fillM {
          val $ = x
          x += 1
          $.optFilter(_ <= 5).hoistId
        }.toStream shouldEventuallyReturn 1.to(5).toStream
      }
      "infinite" in {
        var x = 1
        RichStreamT.fillM {
          x += 1
          OptionT.some(x - 1)
        }.take(5).toStream shouldEventuallyReturn 1.to(5).toStream
      }
    }

    "fromStream" in {
      RichStreamT.fromStream[Future, Int](Stream.iterate(1)(_ + 1))
          .take(3) valueShouldEventuallyReturn Stream(1, 2, 3)
    }

    "fromEvaluatedIterable" - {
      "empty" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](Nil).valueShouldEventuallyReturn(Stream.empty)
      }
      "single element" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](Vector(1)).valueShouldEventuallyReturn(Stream(1))
      }
      "multiple element" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](List(1, 2)).valueShouldEventuallyReturn(Stream(1, 2))
      }
    }
  }
}
