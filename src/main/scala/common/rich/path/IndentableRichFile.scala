package common.rich.path

import java.io.File

/** A special RichFile that supports appending lines in different levels of indentations. */
class IndentableRichFile private (f: File, indentLevel: Int) extends RichFile(f) {
  require(indentLevel >= 0, "indentLevel cannot be negative")
  def this(f: File) = this(f, 0)

  def increaseIndent = new IndentableRichFile(f, indentLevel + 1)
  def decreaseIndent = new IndentableRichFile(f, indentLevel - 1)
  def resetIndentation() = new IndentableRichFile(f, 0)
  override def clear(): IndentableRichFile = {
    super.clear()
    this.resetIndentation()
  }
  override def appendLine(s: String): this.type = {
    super.appendLine(("\t" * indentLevel) + s)
    this
  }
}
