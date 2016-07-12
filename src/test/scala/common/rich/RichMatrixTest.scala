package common.rich

import Jama.Matrix
import org.scalatest.FlatSpec
import org.scalatest.matchers.{BeMatcher, MatchResult, ShouldMatchers}
import stats.RichMatrix
import stats.RichMatrix._

class RichMatrixTest extends FlatSpec with ShouldMatchers {
  val matrix = RichMatrix.apply(Seq(Seq(1, 2), Seq(3, 4)))
  val eye = RichMatrix.eye(2)
  def equalTo(m: Matrix) = new BeMatcher[Matrix] {
    def apply(left: Matrix): MatchResult = {
      return MatchResult(m.rows == left.rows, "matrices are different", "matrices are the same")
    }
  }

  "rows" should "return the rows" in {
    matrix.rows should be === Seq(Seq(1, 2), Seq(3, 4))
  }

  "cols" should "return the columns" in {
    matrix.cols should be === Seq(Seq(1, 3), Seq(2, 4))
  }

  "eye" should "return the unit matrix" in {
    eye.rows should be === Seq(Seq(1, 0), Seq(0, 1))
  }

  "zeroes" should "a zero matrix" in {
    RichMatrix.zeroes(2).rows should be === Seq(Seq(0, 0), Seq(0, 0))
  }

  "times" should "return the matrix when multiplying with the unit matrix" in {
    eye * matrix should be(equalTo(matrix))
    matrix * eye should be(equalTo(matrix))
    eye * eye should be(equalTo(RichMatrix.eye(2)))
  }

  it should "return the multiplied matrix" in {
    matrix * matrix should be(equalTo(RichMatrix.apply(Seq(Seq(7, 10), Seq(15, 22)))))
  }

  "timesCol" should "return the sequence for the unit matrix" in {
    eye timesCol Seq(1, 2) should be === Seq(1, 2)
  }

  "timesCol" should "return the correct value in" in {
    matrix timesCol Seq(5, 6) should be === Seq(17, 39)
  }

  "mapRows" should "map the rows" in {
    matrix.mapRows(_ map (_ * 2)) * eye should be(equalTo(RichMatrix.apply(Seq(Seq(2, 4), Seq(6, 8)))))
  }

  "map cells" should "map all the cells" in {
    matrix.mapCells(_ * 2).rows should be === Seq(Seq(2, 4), Seq(6, 8))
  }

  "update rows" should "update the rows" in {
    matrix.updateRow(0, Seq(5, 6))
    matrix.rows should be === Seq(Seq(5, 6), Seq(3, 4))
  }
}
