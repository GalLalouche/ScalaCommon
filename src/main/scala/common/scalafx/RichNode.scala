package common.scalafx

import scalafx.application.Platform.runLater
import scalafx.scene.Node

import common.rich.RichT.richT

object RichNode {
  implicit class richNode(n: Node) {
    def setStyleLater(style: String): Unit = runLater(n.style = style)
    def setBackgroundColor(color: String): Unit = setStyleLater(Styles.backgroundColor(color))
    def setBaseColor(color: String): Unit = setStyleLater(Styles.baseColor(color))
    def setFontWeight(style: String): Unit = setStyleLater(Styles.fontWeight(style))
    def setFontSize(size: Int): Unit = setStyleLater(Styles.fontSize(size))
    def makeBold(): Node = n.<|(_.setFontWeight("bold"))
  }
}
