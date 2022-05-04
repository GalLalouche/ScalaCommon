package common.scalafx

import scalafx.geometry.Insets

object Builders {
  def insets(top: Int = 0, right: Int = 0, bottom: Int = 0, left: Int = 0): Insets =
    Insets(top = top, right = right, bottom = bottom, left = left)
}
