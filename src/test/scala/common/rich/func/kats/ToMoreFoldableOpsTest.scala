package common.rich.func.kats

import org.scalatest.FreeSpec
import org.scalatest.OptionValues.convertOptionToValuable

import common.rich.func.kats.PlainSeqInstances.plainSeqInstances
import common.rich.func.kats.ToMoreFoldableOps._

import common.test.AuxSpecs

class ToMoreFoldableOpsTest extends FreeSpec with AuxSpecs {
  "mapHeadOrElse" - {
    "when Some" in {
      Vector(1).mapHeadOrElse(_ + 1, ???) shouldReturn 2
    }
    "when None" in {
      Nil.mapHeadOrElse((_: Int) => ???, 2) shouldReturn 2
    }
  }
  "head" in { Vector(1, 2, 3).head shouldReturn 1 }
  "headOpt" - {
    "empty" in { Vector().headOpt shouldReturn None }
    "nonEmpty" in { Vector(1, 2, 3).headOpt.value shouldReturn 1 }
    "infinite" in { Stream.iterate(1)(_ + 1).headOpt.value shouldReturn 1 }
  }
  "nth" - {
    "negative index" in { Vector(1, 2, 3).nth(-1) shouldReturn None }
    "zero index" in { Vector(1, 2, 3).nth(0).value shouldReturn 1 }
    "exists" in { Vector(1, 2, 3).nth(1).value shouldReturn 2 }
    "last" in { Vector(1, 2, 3, 4, 5).nth(4).value shouldReturn 5 }
    "out of bounds" in { Vector(1, 2, 3, 4, 5).nth(5) shouldReturn None }
    "infinite" in { Stream.iterate(0)(_ + 1).nth(5).value shouldReturn 5 }
  }

  "unzipEithers" in {
    val expected: (Vector[String], Vector[Int]) = (Vector("foo", "bar"), Vector(42, 54))
    val vector: Seq[Either[String, Int]] = Vector(Left("foo"), Right(42), Left("bar"), Right(54))
    EitherFoldableOps(vector).partitionEithers shouldReturn expected
  }
}
