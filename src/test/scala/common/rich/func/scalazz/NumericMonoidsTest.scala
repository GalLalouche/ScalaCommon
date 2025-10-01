package common.rich.func.scalazz

import org.scalatest.FreeSpec

import scalaz.{Foldable, Monoid}
import scalaz.std.vector.vectorInstance

import common.test.AuxSpecs

class NumericMonoidsTest extends FreeSpec with AuxSpecs {
  private def fold[A: Monoid](v: Vector[A]): A = Foldable[Vector].fold(v)
  "sum" - {
    import NumericMonoids.SumNumeric
    "ints" in {
      fold(Vector(1, 2, 3, 4)) shouldReturn 10
    }
    "doubles" in {
      fold(Vector(1.0, 2.0, 3.0, 4.0)) shouldReturn 10.0
    }
    "longs" in {
      fold(Vector(1L, 2L, 3L, 4L)) shouldReturn 10L
    }
  }
  "product" - {
    import NumericMonoids.ProductNumeric
    "ints" in {
      fold(Vector(1, 2, 3, 4)) shouldReturn 24
    }
    "doubles" in {
      fold(Vector(1.0, 2.0, 3.0, 4.0)) shouldReturn 24.0
    }
    "longs" in {
      fold(Vector(1L, 2L, 3L, 4L)) shouldReturn 24L
    }
  }
}
