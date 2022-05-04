package common.scalafx

object Styles {
  // Not all of these used, as they are copy-pasted from another projected
  def style(property: String, value: String): String = s"$property: $value"
  def fontWeight(value: String): String = style("-fx-font-weight", value)
  def fontSize(value: Int): String = style("-fx-font-size", value + "pt")
  def backgroundColor(value: String): String = style("-fx-background-color", value)
  def baseColor(value: String): String = style("-fx-base", value)
}
