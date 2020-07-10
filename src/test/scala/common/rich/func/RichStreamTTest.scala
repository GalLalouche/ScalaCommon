package common.rich.func

import org.scalatest.AsyncFreeSpec

import scala.concurrent.Future

import scalaz.{OptionT, StreamT}
import common.rich.func.BetterFutureInstances._

import common.test.AsyncAuxSpecs

class RichStreamTTest extends AsyncFreeSpec with AsyncAuxSpecs {
  "Object methods" - {
    "iterateM" - {
      "basic" in {
        RichStreamT.iterateM(1)(e => RichOptionT.when(e < 5)(Future successful e + 1))
            .toStream
            .map(_ shouldReturn 1.to(5).toStream)
      }
      "infinite" in {
        RichStreamT.iterateM(1)(e => RichOptionT.pointSome[Future].apply(e + 1)).take(5)
            .toStream
            .map(_ shouldReturn 1.to(5).toStream)
      }
    }

    "fromStream" in {
      RichStreamT.fromStream[Future, Int](Stream.iterate(1)(_ + 1))
          .take(3)
          .mapValue(_ shouldReturn Stream(1, 2, 3))
    }

    "fromEvaluatedIterable" - {
      "empty" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](Nil).mapValue(_ shouldReturn Stream.empty)
      }
      "single element" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](Vector(1)).mapValue(_ shouldReturn Stream(1))
      }
      "multiple element" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](List(1, 2)).mapValue(_ shouldReturn Stream(1, 2))
      }
    }
  }

  "richStreamT" - {
    import RichStreamT._
    "oMapM" - {
      "empty" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](Nil).oMapM[Int](_ => ???).mapValue(_ shouldBe empty)
      }
      def cubeEvens(e: Int) =
        if (e % 2 == 0)
          RichOptionT.pointSome[Future].apply(e * e * e)
        else
          OptionT.none[Future, Int]
      "multiple elements" in {
        RichStreamT.fromEvaluatedIterable[Future, Int](List(1, 2, 3, 4))
            .oMapM(cubeEvens).mapValue(_ shouldReturn Stream(8, 64))
      }
      "infinite" in {
        // Checks type inferrence too!
        val $: StreamT[Future, Int] = RichStreamT.fromStream(Stream.iterate(1)(_ + 1))
        $.oMapM(cubeEvens)
            .take(3)
            .mapValue(_ shouldReturn Stream(8, 64, 216))
      }
    }
  }
}
